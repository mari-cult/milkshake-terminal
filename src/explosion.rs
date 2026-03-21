use bytemuck::{Pod, Zeroable};
use std::time::Instant;
use wgpu::{
    ColorTargetState, FragmentState, MultisampleState, PipelineCompilationOptions,
    PipelineLayoutDescriptor, PrimitiveState, RenderPipeline, RenderPipelineDescriptor,
    ShaderModuleDescriptor, ShaderSource, TextureFormat, TextureSampleType, TextureViewDescriptor,
    TextureViewDimension, VertexAttribute, VertexBufferLayout, VertexFormat, VertexState,
    VertexStepMode,
};

const SHARD_SHADER: &str = r#"
struct VertexInput {
    @location(0) pos: vec2<f32>,
    @location(1) uv: vec2<f32>,
    @location(2) local_uv: vec2<f32>,
    @location(3) opacity: f32,
};
struct VertexOutput {
    @builtin(position) pos: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) local_uv: vec2<f32>,
    @location(2) opacity: f32,
};
@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.pos = vec4<f32>(in.pos, 0.0, 1.0);
    out.uv = in.uv;
    out.local_uv = in.local_uv;
    out.opacity = in.opacity;
    return out;
}
@group(0) @binding(0) var frame_tex: texture_2d<f32>;
@group(0) @binding(1) var frame_sampler: sampler;
@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let color = textureSample(frame_tex, frame_sampler, in.uv);
    let edge_x = min(in.local_uv.x, 1.0 - in.local_uv.x);
    let edge_y = min(in.local_uv.y, 1.0 - in.local_uv.y);
    let edge = min(edge_x, edge_y);
    let border = 1.0 - smoothstep(0.0, 0.03, edge);
    let border_color = vec3<f32>(0.45, 0.5, 0.6);
    let rgb = color.rgb;
    let a = in.opacity;
    return vec4<f32>(rgb, a);
}
"#;

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
struct ShardVertex {
    pos: [f32; 2],
    uv: [f32; 2],
    local_uv: [f32; 2],
    opacity: f32,
}

struct Shard {
    cx: f32,
    cy: f32,
    hw: f32,
    hh: f32,
    uv_left: f32,
    uv_top: f32,
    uv_right: f32,
    uv_bottom: f32,
    tx: f32,
    ty: f32,
    vx: f32,
    vy: f32,
    angle: f32,
    angular_vel: f32,
    opacity: f32,
}

pub struct ExplosionEffect {
    shards: Vec<Shard>,
    pipeline: RenderPipeline,
    bind_group: wgpu::BindGroup,
    vertex_buffer: wgpu::Buffer,
    vertex_capacity: usize,
    start_time: Instant,
    _snapshot_texture: wgpu::Texture,
    pub finished: bool,
    scale_x: f32,
    scale_y: f32,
    offset_x: f32,
    offset_y: f32,
    shake_x: f32,
    shake_y: f32,
    screen_w: f32,
    screen_h: f32,
}

impl ExplosionEffect {
    const DURATION: f32 = 1.2;
    const GRID_X: u32 = 10;
    const GRID_Y: u32 = 8;
    const GRAVITY: f32 = 4.0;
    const SHAKE_AMP: f32 = 14.0;
    const SHAKE_FREQ: f32 = 40.0;

    pub fn new(
        device: &wgpu::Device,
        format: TextureFormat,
        snapshot_texture: wgpu::Texture,
        scale_x: f32,
        scale_y: f32,
        offset_x: f32,
        offset_y: f32,
        screen_w: f32,
        screen_h: f32,
    ) -> Self {
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("explosion-shader"),
            source: ShaderSource::Wgsl(SHARD_SHADER.into()),
        });
        let bgl = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("explosion-bgl"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: true },
                        view_dimension: TextureViewDimension::D2,
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
        let pl = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("explosion-pl"),
            bind_group_layouts: &[Some(&bgl)],
            immediate_size: 0,
        });
        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("explosion-pipeline"),
            layout: Some(&pl),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<ShardVertex>() as u64,
                    step_mode: VertexStepMode::Vertex,
                    attributes: &[
                        VertexAttribute {
                            format: VertexFormat::Float32x2,
                            offset: 0,
                            shader_location: 0,
                        },
                        VertexAttribute {
                            format: VertexFormat::Float32x2,
                            offset: 8,
                            shader_location: 1,
                        },
                        VertexAttribute {
                            format: VertexFormat::Float32x2,
                            offset: 16,
                            shader_location: 2,
                        },
                        VertexAttribute {
                            format: VertexFormat::Float32,
                            offset: 24,
                            shader_location: 3,
                        },
                    ],
                }],
            },
            primitive: PrimitiveState::default(),
            depth_stencil: None,
            multisample: MultisampleState::default(),
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                targets: &[Some(ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        });

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("explosion-sampler"),
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });
        let view = snapshot_texture.create_view(&TextureViewDescriptor::default());
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("explosion-bg"),
            layout: &bgl,
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

        let mut shards = Vec::with_capacity((Self::GRID_X * Self::GRID_Y) as usize);
        let sw = 2.0 / Self::GRID_X as f32;
        let sh = 2.0 / Self::GRID_Y as f32;
        let mut seed: u32 = 0xDEAD_BEEF;
        let mut rng = |lo: f32, hi: f32| -> f32 {
            seed ^= seed << 13;
            seed ^= seed >> 17;
            seed ^= seed << 5;
            lo + (hi - lo) * (seed & 0xFFFF) as f32 / 65535.0
        };
        for gy in 0..Self::GRID_Y {
            for gx in 0..Self::GRID_X {
                let cx = -1.0 + sw * (gx as f32 + 0.5);
                let cy = 1.0 - sh * (gy as f32 + 0.5);
                let dir_x = cx + rng(-0.15, 0.15);
                let dir_y = cy + rng(-0.15, 0.15);
                let speed = rng(1.5, 4.5);
                shards.push(Shard {
                    cx,
                    cy,
                    hw: sw * 0.5,
                    hh: sh * 0.5,
                    uv_left: gx as f32 / Self::GRID_X as f32,
                    uv_top: gy as f32 / Self::GRID_Y as f32,
                    uv_right: (gx + 1) as f32 / Self::GRID_X as f32,
                    uv_bottom: (gy + 1) as f32 / Self::GRID_Y as f32,
                    tx: 0.0,
                    ty: 0.0,
                    vx: dir_x * speed,
                    vy: dir_y * speed + rng(0.5, 2.0),
                    angle: 0.0,
                    angular_vel: rng(-8.0, 8.0),
                    opacity: 1.0,
                });
            }
        }

        let vertex_capacity = shards.len() * 6;
        let vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("explosion-vb"),
            size: (vertex_capacity * std::mem::size_of::<ShardVertex>()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        Self {
            shards,
            pipeline,
            bind_group,
            vertex_buffer,
            vertex_capacity,
            start_time: Instant::now(),
            _snapshot_texture: snapshot_texture,
            finished: false,
            scale_x,
            scale_y,
            offset_x,
            offset_y,
            shake_x: 0.0,
            shake_y: 0.0,
            screen_w,
            screen_h,
        }
    }

    pub fn update(&mut self) {
        let elapsed = self.start_time.elapsed().as_secs_f32();
        if elapsed >= Self::DURATION {
            self.finished = true;
            return;
        }

        let dt = 1.0 / 60.0_f32;
        let progress = elapsed / Self::DURATION;

        for shard in &mut self.shards {
            shard.vy -= Self::GRAVITY * dt;
            shard.tx += shard.vx * dt;
            shard.ty += shard.vy * dt;
            shard.angle += shard.angular_vel * dt;
            shard.opacity = if progress < 0.3 {
                1.0
            } else {
                (1.0 - (progress - 0.3) / 0.7).max(0.0)
            };
        }

        let shake_decay = if progress < 0.6 {
            1.0 - progress / 0.6
        } else {
            0.0
        };
        let ax = Self::SHAKE_AMP * 2.0 / self.screen_w;
        let ay = Self::SHAKE_AMP * 2.0 / self.screen_h;
        self.shake_x = (elapsed * Self::SHAKE_FREQ).sin() * ax * shake_decay;
        self.shake_y = (elapsed * Self::SHAKE_FREQ * 1.3).cos() * ay * shake_decay;
    }

    pub fn prepare(&mut self, device: &wgpu::Device, queue: &wgpu::Queue) {
        let mut verts = Vec::with_capacity(self.shards.len() * 6);
        for shard in &self.shards {
            if shard.opacity <= 0.0 {
                continue;
            }
            let (sin_a, cos_a) = shard.angle.sin_cos();
            let corners = [
                (-shard.hw, shard.hh),
                (shard.hw, shard.hh),
                (shard.hw, -shard.hh),
                (-shard.hw, -shard.hh),
            ];
            let uvs = [
                [shard.uv_left, shard.uv_top],
                [shard.uv_right, shard.uv_top],
                [shard.uv_right, shard.uv_bottom],
                [shard.uv_left, shard.uv_bottom],
            ];
            let luvs: [[f32; 2]; 4] = [[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0]];
            let xf = |lx: f32, ly: f32| -> [f32; 2] {
                let rx = lx * cos_a - ly * sin_a;
                let ry = lx * sin_a + ly * cos_a;
                let x = shard.cx + shard.tx + rx;
                let y = shard.cy + shard.ty + ry;
                [
                    x * self.scale_x + self.offset_x + self.shake_x,
                    y * self.scale_y + self.offset_y + self.shake_y,
                ]
            };
            let p = [
                xf(corners[0].0, corners[0].1),
                xf(corners[1].0, corners[1].1),
                xf(corners[2].0, corners[2].1),
                xf(corners[3].0, corners[3].1),
            ];
            let o = shard.opacity;
            for &[a, b, c] in &[[0, 1, 2], [0, 2, 3]] {
                verts.push(ShardVertex {
                    pos: p[a],
                    uv: uvs[a],
                    local_uv: luvs[a],
                    opacity: o,
                });
                verts.push(ShardVertex {
                    pos: p[b],
                    uv: uvs[b],
                    local_uv: luvs[b],
                    opacity: o,
                });
                verts.push(ShardVertex {
                    pos: p[c],
                    uv: uvs[c],
                    local_uv: luvs[c],
                    opacity: o,
                });
            }
        }
        if verts.len() > self.vertex_capacity {
            self.vertex_capacity = verts.len().next_power_of_two();
            self.vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("explosion-vb"),
                size: (self.vertex_capacity * std::mem::size_of::<ShardVertex>()) as u64,
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
        }
        if !verts.is_empty() {
            queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(&verts));
        }
    }

    pub fn render<'a>(&'a self, pass: &mut wgpu::RenderPass<'a>) {
        let n = self.shards.iter().filter(|s| s.opacity > 0.0).count() as u32 * 6;
        if n == 0 {
            return;
        }
        pass.set_pipeline(&self.pipeline);
        pass.set_bind_group(0, &self.bind_group, &[]);
        pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
        pass.draw(0..n, 0..1);
    }
}
