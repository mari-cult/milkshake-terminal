use std::collections::HashMap;
use ttf_parser::{Face, OutlineBuilder};

use rayon::prelude::*;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::PathBuf;

use crate::FONT_DATA;

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct TextVertex {
    pub pos: [f32; 2],
    pub uv: [f32; 2],
    pub color: [f32; 4],
}

#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct GlyphInfo {
    pub uv_min: [f32; 2],
    pub uv_max: [f32; 2],
    pub offset: [f32; 2],
    pub size: [f32; 2],
    pub advance: f32,
}

pub struct FontAtlas {
    pub texture: wgpu::Texture,
    pub bind_group: wgpu::BindGroup,
    pub glyphs: HashMap<u16, GlyphInfo>,
    pub units_per_em: f32,
    pub ascender: f32,
    pub descender: f32,
    pub padding: f32,
    pub ascii_cache: [Option<GlyphInfo>; 128],
}

struct PathSegment {
    p0: [f32; 2],
    p1: [f32; 2],
}

struct Flattener {
    segments: Vec<PathSegment>,
    current: [f32; 2],
    start: [f32; 2],
}

impl Flattener {
    fn new() -> Self {
        Self {
            segments: Vec::new(),
            current: [0.0, 0.0],
            start: [0.0, 0.0],
        }
    }
}

impl OutlineBuilder for Flattener {
    fn move_to(&mut self, x: f32, y: f32) {
        self.current = [x, y];
        self.start = [x, y];
    }
    fn line_to(&mut self, x: f32, y: f32) {
        self.segments.push(PathSegment {
            p0: self.current,
            p1: [x, y],
        });
        self.current = [x, y];
    }
    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        let n = 8;
        for i in 1..=n {
            let t = i as f32 / n as f32;
            let mt = 1.0 - t;
            let px = mt * mt * self.current[0] + 2.0 * mt * t * x1 + t * t * x;
            let py = mt * mt * self.current[1] + 2.0 * mt * t * y1 + t * t * y;
            self.segments.push(PathSegment {
                p0: self.current,
                p1: [px, py],
            });
            self.current = [px, py];
        }
    }
    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        let n = 16;
        for i in 1..=n {
            let t = i as f32 / n as f32;
            let mt = 1.0 - t;
            let px = mt * mt * mt * self.current[0]
                + 3.0 * mt * mt * t * x1
                + 3.0 * mt * t * t * x2
                + t * t * t * x;
            let py = mt * mt * mt * self.current[1]
                + 3.0 * mt * mt * t * y1
                + 3.0 * mt * t * t * y2
                + t * t * t * y;
            self.segments.push(PathSegment {
                p0: self.current,
                p1: [px, py],
            });
            self.current = [px, py];
        }
    }
    fn close(&mut self) {
        self.segments.push(PathSegment {
            p0: self.current,
            p1: self.start,
        });
        self.current = self.start;
    }
}

fn point_line_distance(px: f32, py: f32, x0: f32, y0: f32, x1: f32, y1: f32) -> f32 {
    let dx = x1 - x0;
    let dy = y1 - y0;
    let l2 = dx * dx + dy * dy;
    if l2 == 0.0 {
        return ((px - x0) * (px - x0) + (py - y0) * (py - y0)).sqrt();
    }
    let t = ((px - x0) * dx + (py - y0) * dy) / l2;
    let t = t.clamp(0.0, 1.0);
    let proj_x = x0 + t * dx;
    let proj_y = y0 + t * dy;
    ((px - proj_x) * (px - proj_x) + (py - proj_y) * (py - proj_y)).sqrt()
}

pub fn generate_atlas(device: &wgpu::Device, queue: &wgpu::Queue, font_data: &[u8]) -> FontAtlas {
    let mut hasher = Sha256::new();
    hasher.update(font_data);
    let font_hash = hasher.finalize();
    let cache_dir = if cfg!(target_os = "macos") {
        std::env::var("HOME").map(|h| PathBuf::from(h).join("Library/Caches/milkshake-terminal"))
    } else {
        std::env::var("XDG_CACHE_HOME")
            .map(PathBuf::from)
            .or_else(|_| {
                std::env::var("HOME").map(|h| PathBuf::from(h).join(".cache/milkshake-terminal"))
            })
    }
    .unwrap_or_else(|_| PathBuf::from("/tmp/milkshake-terminal"));
    let cache_path = cache_dir.join(format!("{:x}.bin", font_hash));

    if let Ok(cached_data) = fs::read(&cache_path)
        && let Some(atlas) = load_atlas_from_cache(device, queue, &cached_data)
    {
        return atlas;
    }

    let face = Face::parse(font_data, 4).unwrap_or_else(|_| Face::parse(font_data, 0).unwrap());

    let units_per_em = face.units_per_em() as f32;
    let ascender = face.ascender() as f32;
    let descender = face.descender() as f32;

    let grid_size: u32 = 42; // cells
    let cell_res: u32 = 48; // pixels per cell side
    let padding_units = units_per_em * 0.15; // SDF padding

    let atlas_width = grid_size * cell_res;
    let atlas_height = grid_size * cell_res;
    let mut pixels = vec![0u8; (atlas_width * atlas_height) as usize];

    let total_face_glyphs = face.number_of_glyphs();
    let num_glyphs = total_face_glyphs.min((grid_size * grid_size) as u16);

    let glyph_ids: Vec<u16> = (0..num_glyphs).collect();

    let results: Vec<(u16, GlyphInfo, Vec<u8>)> = glyph_ids
        .into_par_iter()
        .map(|glyph_id| {
            let id = ttf_parser::GlyphId(glyph_id);
            let mut flattener = Flattener::new();
            let bbox_opt = face.outline_glyph(id, &mut flattener);
            let advance = face.glyph_hor_advance(id).unwrap_or(0) as f32;

            let col = glyph_id as u32 % grid_size;
            let row = glyph_id as u32 / grid_size;

            let cx = col * cell_res;
            let cy = row * cell_res;

            if let Some(bbox) = bbox_opt {
                let mut glyph_pixels = vec![0u8; (cell_res * cell_res) as usize];

                let bx_min = bbox.x_min as f32 - padding_units;
                let by_min = bbox.y_min as f32 - padding_units;
                let bx_max = bbox.x_max as f32 + padding_units;
                let by_max = bbox.y_max as f32 + padding_units;

                for y in 0..cell_res {
                    for x in 0..cell_res {
                        let fx = bx_min + (x as f32 / (cell_res - 1) as f32) * (bx_max - bx_min);
                        let fy = by_max - (y as f32 / (cell_res - 1) as f32) * (by_max - by_min);

                        let mut min_dist_sq: f32 = f32::MAX;
                        let mut winding_number = 0;

                        for seg in &flattener.segments {
                            let d = point_line_distance(
                                fx, fy, seg.p0[0], seg.p0[1], seg.p1[0], seg.p1[1],
                            );
                            min_dist_sq = min_dist_sq.min(d * d);

                            if ((seg.p0[1] <= fy) && (seg.p1[1] > fy))
                                || ((seg.p1[1] <= fy) && (seg.p0[1] > fy))
                            {
                                let intersect_x = seg.p0[0]
                                    + (fy - seg.p0[1]) / (seg.p1[1] - seg.p0[1])
                                        * (seg.p1[0] - seg.p0[0]);
                                if intersect_x > fx {
                                    if seg.p1[1] > seg.p0[1] {
                                        winding_number += 1;
                                    } else {
                                        winding_number -= 1;
                                    }
                                }
                            }
                        }

                        let sign = if winding_number != 0 { 1.0 } else { -1.0 };
                        let d = (min_dist_sq.sqrt() * sign) / padding_units;
                        let d_mapped = (d * 0.5 + 0.5).clamp(0.0, 1.0);
                        glyph_pixels[(y * cell_res + x) as usize] = (d_mapped * 255.0) as u8;
                    }
                }

                (
                    glyph_id,
                    GlyphInfo {
                        uv_min: [
                            cx as f32 / atlas_width as f32,
                            cy as f32 / atlas_height as f32,
                        ],
                        uv_max: [
                            (cx + cell_res) as f32 / atlas_width as f32,
                            (cy + cell_res) as f32 / atlas_height as f32,
                        ],
                        offset: [bx_min, by_min],
                        size: [bx_max - bx_min, by_max - by_min],
                        advance,
                    },
                    glyph_pixels,
                )
            } else {
                (
                    glyph_id,
                    GlyphInfo {
                        uv_min: [0.0, 0.0],
                        uv_max: [0.0, 0.0],
                        offset: [0.0, 0.0],
                        size: [0.0, 0.0],
                        advance,
                    },
                    vec![0u8; (cell_res * cell_res) as usize],
                )
            }
        })
        .collect();

    let mut glyphs_info = HashMap::new();
    for (id, info, glyph_pixels) in results {
        glyphs_info.insert(id, info);
        let col = id as u32 % grid_size;
        let row = id as u32 / grid_size;
        let cx = col * cell_res;
        let cy = row * cell_res;
        for y in 0..cell_res {
            for x in 0..cell_res {
                pixels[((cy + y) * atlas_width + (cx + x)) as usize] =
                    glyph_pixels[(y * cell_res + x) as usize];
            }
        }
    }

    let texture_size = wgpu::Extent3d {
        width: atlas_width,
        height: atlas_height,
        depth_or_array_layers: 1,
    };

    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("SDF Atlas"),
        size: texture_size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::R8Unorm,
        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        view_formats: &[],
    });

    queue.write_texture(
        texture.as_image_copy(),
        &pixels,
        wgpu::TexelCopyBufferLayout {
            offset: 0,
            bytes_per_row: Some(atlas_width),
            rows_per_image: Some(atlas_height),
        },
        texture_size,
    );

    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        ..Default::default()
    });

    let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: Some("SDF Atlas Bind Group Layout"),
        entries: &[
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Texture {
                    sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    view_dimension: wgpu::TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            },
            wgpu::BindGroupLayoutEntry {
                binding: 1,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                count: None,
            },
        ],
    });

    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("SDF Atlas Bind Group"),
        layout: &bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&view),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(&sampler),
            },
        ],
    });

    let mut cache_data = Vec::new();
    cache_data.extend_from_slice(b"MSAT");
    cache_data.extend_from_slice(&2u32.to_le_bytes());
    cache_data.extend_from_slice(&(glyphs_info.len() as u32).to_le_bytes());
    cache_data.extend_from_slice(&units_per_em.to_le_bytes());
    cache_data.extend_from_slice(&ascender.to_le_bytes());
    cache_data.extend_from_slice(&descender.to_le_bytes());
    cache_data.extend_from_slice(&padding_units.to_le_bytes());
    cache_data.extend_from_slice(&atlas_width.to_le_bytes());
    cache_data.extend_from_slice(&atlas_height.to_le_bytes());

    for (&id, info) in &glyphs_info {
        cache_data.extend_from_slice(&id.to_le_bytes());
        cache_data.extend_from_slice(&[0u8, 0u8]); // Padding for alignment
        cache_data.extend_from_slice(bytemuck::bytes_of(info));
    }
    cache_data.extend_from_slice(&pixels);

    let _ = fs::create_dir_all(&cache_dir);
    let _ = fs::write(&cache_path, cache_data);

    let mut ascii_cache = [None; 128];
    for ch in 0..128u8 {
        if let Some(id) = face.glyph_index(ch as char)
            && let Some(info) = glyphs_info.get(&id.0)
        {
            ascii_cache[ch as usize] = Some(*info);
        }
    }

    FontAtlas {
        texture,
        bind_group,
        glyphs: glyphs_info,
        units_per_em,
        ascender,
        descender,
        padding: padding_units,
        ascii_cache,
    }
}

fn load_atlas_from_cache(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    data: &[u8],
) -> Option<FontAtlas> {
    if data.len() < 40 || &data[0..4] != b"MSAT" {
        return None;
    }
    let version = u32::from_le_bytes(data[4..8].try_into().ok()?);
    if version != 2 {
        return None;
    }

    let num_glyphs = u32::from_le_bytes(data[8..12].try_into().ok()?);
    let units_per_em = f32::from_le_bytes(data[12..16].try_into().ok()?);
    let ascender = f32::from_le_bytes(data[16..20].try_into().ok()?);
    let descender = f32::from_le_bytes(data[20..24].try_into().ok()?);
    let padding = f32::from_le_bytes(data[24..28].try_into().ok()?);
    let atlas_width = u32::from_le_bytes(data[28..32].try_into().ok()?);
    let atlas_height = u32::from_le_bytes(data[32..36].try_into().ok()?);

    let mut glyphs = HashMap::new();
    let mut offset = 36;
    for _ in 0..num_glyphs {
        let id = u16::from_le_bytes(data[offset..offset + 2].try_into().ok()?);
        offset += 4; // Skip ID and padding
        let info_len = std::mem::size_of::<GlyphInfo>();
        let info: GlyphInfo = *bytemuck::from_bytes(&data[offset..offset + info_len]);
        offset += info_len;
        glyphs.insert(id, info);
    }

    let pixels = &data[offset..];
    if pixels.len() != (atlas_width * atlas_height) as usize {
        return None;
    }

    let texture_size = wgpu::Extent3d {
        width: atlas_width,
        height: atlas_height,
        depth_or_array_layers: 1,
    };
    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("SDF Atlas Cached"),
        size: texture_size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::R8Unorm,
        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        view_formats: &[],
    });

    queue.write_texture(
        texture.as_image_copy(),
        pixels,
        wgpu::TexelCopyBufferLayout {
            offset: 0,
            bytes_per_row: Some(atlas_width),
            rows_per_image: Some(atlas_height),
        },
        texture_size,
    );

    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        ..Default::default()
    });

    let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: Some("SDF Atlas Bind Group Layout"),
        entries: &[
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Texture {
                    sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    view_dimension: wgpu::TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            },
            wgpu::BindGroupLayoutEntry {
                binding: 1,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                count: None,
            },
        ],
    });

    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("SDF Atlas Bind Group"),
        layout: &bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&view),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(&sampler),
            },
        ],
    });

    let mut ascii_cache = [None; 128];
    let face = Face::parse(FONT_DATA, 4).unwrap_or_else(|_| Face::parse(FONT_DATA, 0).unwrap());
    for ch in 0..128u8 {
        if let Some(id) = face.glyph_index(ch as char)
            && let Some(info) = glyphs.get(&id.0)
        {
            ascii_cache[ch as usize] = Some(*info);
        }
    }

    Some(FontAtlas {
        texture,
        bind_group,
        glyphs,
        units_per_em,
        ascender,
        descender,
        padding,
        ascii_cache,
    })
}

pub struct SDFTextRenderer {
    pipeline: wgpu::RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    vertex_capacity: usize,
    vertex_count: u32,
    bind_group_layout: wgpu::BindGroupLayout,
}

impl SDFTextRenderer {
    pub fn new(device: &wgpu::Device, format: wgpu::TextureFormat) -> Self {
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("SDF Text Shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("text.wgsl").into()),
        });

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("SDF Atlas Bind Group Layout"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        view_dimension: wgpu::TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("SDF Text Pipeline Layout"),
            bind_group_layouts: &[Some(&bind_group_layout)],
            immediate_size: 0,
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("SDF Text Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: Default::default(),
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: std::mem::size_of::<TextVertex>() as wgpu::BufferAddress,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &[
                        wgpu::VertexAttribute {
                            offset: 0,
                            shader_location: 0,
                            format: wgpu::VertexFormat::Float32x2,
                        },
                        wgpu::VertexAttribute {
                            offset: 8,
                            shader_location: 1,
                            format: wgpu::VertexFormat::Float32x2,
                        },
                        wgpu::VertexAttribute {
                            offset: 16,
                            shader_location: 2,
                            format: wgpu::VertexFormat::Float32x4,
                        },
                    ],
                }],
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                compilation_options: Default::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview_mask: None,
            cache: None,
        });

        let vertex_capacity = 10000;
        let vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("SDF Text Vertex Buffer"),
            size: (vertex_capacity * std::mem::size_of::<TextVertex>()) as wgpu::BufferAddress,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        Self {
            pipeline,
            vertex_buffer,
            vertex_capacity,
            vertex_count: 0,
            bind_group_layout,
        }
    }

    pub fn layout_text(
        atlas: &FontAtlas,
        face: &Face,
        text: &str,
        x: f32,
        y: f32,
        cell_w: f32,
        _cell_h: f32,
        font_size: f32,
        surface_w: f32,
        surface_h: f32,
        color: [f32; 4],
        vertices: &mut Vec<TextVertex>,
    ) {
        let em_scale = font_size / atlas.units_per_em;
        let mut cur_x = x;

        for ch in text.chars() {
            let info = if (ch as u32) < 128 {
                atlas.ascii_cache[ch as usize]
            } else {
                face.glyph_index(ch)
                    .and_then(|id| atlas.glyphs.get(&id.0).copied())
            };

            if let Some(info) = info
                && info.size[0] > 0.0
            {
                let gw = info.size[0] * em_scale;
                let gh = info.size[1] * em_scale;

                let advance_width = info.advance * em_scale;
                let h_padding = (cell_w - advance_width) / 2.0;

                let gx = cur_x + h_padding + info.offset[0] * em_scale;
                let gy = y + (atlas.ascender - info.offset[1] - info.size[1]) * em_scale;

                let x0 = (gx / surface_w) * 2.0 - 1.0;
                let x1 = ((gx + gw) / surface_w) * 2.0 - 1.0;
                let y0 = 1.0 - (gy / surface_h) * 2.0;
                let y1 = 1.0 - ((gy + gh) / surface_h) * 2.0;

                let uv00 = [info.uv_min[0], info.uv_min[1]];
                let uv10 = [info.uv_max[0], info.uv_min[1]];
                let uv11 = [info.uv_max[0], info.uv_max[1]];
                let uv01 = [info.uv_min[0], info.uv_max[1]];

                vertices.extend_from_slice(&[
                    TextVertex {
                        pos: [x0, y0],
                        uv: uv00,
                        color,
                    },
                    TextVertex {
                        pos: [x1, y0],
                        uv: uv10,
                        color,
                    },
                    TextVertex {
                        pos: [x1, y1],
                        uv: uv11,
                        color,
                    },
                    TextVertex {
                        pos: [x0, y0],
                        uv: uv00,
                        color,
                    },
                    TextVertex {
                        pos: [x1, y1],
                        uv: uv11,
                        color,
                    },
                    TextVertex {
                        pos: [x0, y1],
                        uv: uv01,
                        color,
                    },
                ]);
            }
            cur_x += cell_w;
        }
    }

    pub fn prepare(&mut self, device: &wgpu::Device, queue: &wgpu::Queue, vertices: &[TextVertex]) {
        self.vertex_count = vertices.len() as u32;
        if vertices.is_empty() {
            return;
        }

        if vertices.len() > self.vertex_capacity {
            self.vertex_capacity = vertices.len().next_power_of_two();
            self.vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("SDF Text Vertex Buffer"),
                size: (self.vertex_capacity * std::mem::size_of::<TextVertex>()) as u64,
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
        }
        queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(vertices));
    }

    pub fn render<'a>(&'a self, atlas: &'a FontAtlas, pass: &mut wgpu::RenderPass<'a>) {
        if self.vertex_count == 0 {
            return;
        }
        pass.set_pipeline(&self.pipeline);
        pass.set_bind_group(0, &atlas.bind_group, &[]);
        pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
        pass.draw(0..self.vertex_count, 0..1);
    }

    pub fn layout_char(
        atlas: &FontAtlas,
        face: &Face,
        ch: char,
        gx_base: f32,
        gy_base: f32,
        cell_w: f32,
        _cell_h: f32,
        font_size: f32,
        surface_w: f32,
        surface_h: f32,
        color: [f32; 4],
        vertices: &mut Vec<TextVertex>,
    ) {
        let info = if (ch as u32) < 128 {
            atlas.ascii_cache[ch as usize]
        } else {
            face.glyph_index(ch)
                .and_then(|id| atlas.glyphs.get(&id.0).copied())
        };

        if let Some(info) = info
            && info.size[0] > 0.0
        {
            let em_scale = font_size / atlas.units_per_em;
            let gw = info.size[0] * em_scale;
            let gh = info.size[1] * em_scale;

            let advance_width = info.advance * em_scale;
            let h_padding = (cell_w - advance_width) / 2.0;

            let gx = gx_base + h_padding + info.offset[0] * em_scale;
            let gy = gy_base + (atlas.ascender - info.offset[1] - info.size[1]) * em_scale;

            let x0 = (gx / surface_w) * 2.0 - 1.0;
            let x1 = ((gx + gw) / surface_w) * 2.0 - 1.0;
            let y0 = 1.0 - (gy / surface_h) * 2.0;
            let y1 = 1.0 - ((gy + gh) / surface_h) * 2.0;

            let uv00 = [info.uv_min[0], info.uv_min[1]];
            let uv10 = [info.uv_max[0], info.uv_min[1]];
            let uv11 = [info.uv_max[0], info.uv_max[1]];
            let uv01 = [info.uv_min[0], info.uv_max[1]];

            vertices.extend_from_slice(&[
                TextVertex {
                    pos: [x0, y0],
                    uv: uv00,
                    color,
                },
                TextVertex {
                    pos: [x1, y0],
                    uv: uv10,
                    color,
                },
                TextVertex {
                    pos: [x1, y1],
                    uv: uv11,
                    color,
                },
                TextVertex {
                    pos: [x0, y0],
                    uv: uv00,
                    color,
                },
                TextVertex {
                    pos: [x1, y1],
                    uv: uv11,
                    color,
                },
                TextVertex {
                    pos: [x0, y1],
                    uv: uv01,
                    color,
                },
            ]);
        }
    }
}
