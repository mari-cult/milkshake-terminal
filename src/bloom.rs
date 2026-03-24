use crate::config::BloomConfig;
use bytemuck::{Pod, Zeroable};
use wgpu::{
    BindGroupLayout, CommandEncoder, Device, FragmentState, PipelineCompilationOptions,
    PipelineLayoutDescriptor, Queue, RenderPipeline, RenderPipelineDescriptor,
    ShaderModuleDescriptor, ShaderSource, TextureFormat, TextureView, VertexState,
};

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
struct BloomUniforms {
    threshold: f32,
    intensity: f32,
    opacity: f32,
    _pad0: f32,
}

pub struct BloomRenderer {
    extract_pipeline: RenderPipeline,
    blur_h_pipeline: RenderPipeline,
    blur_v_pipeline: RenderPipeline,
    composite_pipeline: RenderPipeline,
    bind_group_layout_scene: BindGroupLayout,
    bind_group_layout_bloom: BindGroupLayout,
    uniform_buffer: wgpu::Buffer,
    tex_extract: Option<wgpu::Texture>,
    tex_blur_h: Option<wgpu::Texture>,
    view_extract: Option<wgpu::TextureView>,
    view_blur_h: Option<wgpu::TextureView>,
    bg_extract: Option<wgpu::BindGroup>,
    bg_blur_h: Option<wgpu::BindGroup>,
    bg_blur_v: Option<wgpu::BindGroup>,
    bg_bloom: Option<wgpu::BindGroup>,
    active_main_view: Option<TextureViewId>,
    format: TextureFormat,
    width: u32,
    height: u32,
}

// Add a dummy ID to track changes
type TextureViewId = u64;

impl BloomRenderer {
    pub fn new(device: &Device, format: TextureFormat) -> Self {
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("Bloom Shaders"),
            source: ShaderSource::Wgsl(include_str!("bloom_shaders.wgsl").into()),
        });

        let bind_group_layout_scene =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("Bloom Scene Bind Group Layout"),
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
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                ],
            });

        let bind_group_layout_bloom =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("Bloom Result Bind Group Layout"),
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

        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Bloom Pipeline Layout"),
            bind_group_layouts: &[Some(&bind_group_layout_scene)],
            immediate_size: 0,
        });

        let composite_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Bloom Composite Pipeline Layout"),
            bind_group_layouts: &[
                Some(&bind_group_layout_scene),
                Some(&bind_group_layout_bloom),
            ],
            immediate_size: 0,
        });

        let extract_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Bloom Extract Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                buffers: &[],
            },
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_extract"),
                compilation_options: PipelineCompilationOptions::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        });

        let blur_h_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Bloom Blur H Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                buffers: &[],
            },
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_blur"),
                compilation_options: PipelineCompilationOptions::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        });

        let blur_v_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Bloom Blur V Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                buffers: &[],
            },
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_blur_v"),
                compilation_options: PipelineCompilationOptions::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        });

        let composite_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("Bloom Composite Pipeline"),
            layout: Some(&composite_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                buffers: &[],
            },
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_composite"),
                compilation_options: PipelineCompilationOptions::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        });

        let uniform_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Bloom Uniform Buffer"),
            size: std::mem::size_of::<BloomUniforms>() as u64,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        Self {
            extract_pipeline,
            blur_h_pipeline,
            blur_v_pipeline,
            composite_pipeline,
            bind_group_layout_scene,
            bind_group_layout_bloom,
            uniform_buffer,
            tex_extract: None,
            tex_blur_h: None,
            view_extract: None,
            view_blur_h: None,
            bg_extract: None,
            bg_blur_h: None,
            bg_blur_v: None,
            bg_bloom: None,
            active_main_view: None,
            format,
            width: 0,
            height: 0,
        }
    }

    fn ensure_textures(&mut self, device: &Device, width: u32, height: u32) {
        if self.width == width && self.height == height && self.tex_extract.is_some() {
            return;
        }

        let tex_desc = wgpu::TextureDescriptor {
            label: Some("Bloom Temp Texture"),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: self.format,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        };

        let tex_extract = device.create_texture(&tex_desc);
        let tex_blur_h = device.create_texture(&tex_desc);
        let view_extract = tex_extract.create_view(&wgpu::TextureViewDescriptor::default());
        let view_blur_h = tex_blur_h.create_view(&wgpu::TextureViewDescriptor::default());

        self.tex_extract = Some(tex_extract);
        self.tex_blur_h = Some(tex_blur_h);
        self.view_extract = Some(view_extract);
        self.view_blur_h = Some(view_blur_h);
        self.width = width;
        self.height = height;
        self.active_main_view = None; // Force bind group recreation
    }

    pub fn render(
        &mut self,
        device: &Device,
        queue: &Queue,
        encoder: &mut CommandEncoder,
        main_scene_view: &TextureView,
        target_view: &TextureView,
        width: u32,
        height: u32,
        config: &BloomConfig,
        opacity: f32,
    ) {
        self.ensure_textures(device, width, height);

        // Update uniforms
        queue.write_buffer(
            &self.uniform_buffer,
            0,
            bytemuck::cast_slice(&[BloomUniforms {
                threshold: config.threshold,
                intensity: config.intensity,
                opacity,
                _pad0: 0.0,
            }]),
        );

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        // Recreate bind groups only if necessary (simplified for now to avoid ID complexity)
        let bg_extract = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Bloom Extract BG"),
            layout: &self.bind_group_layout_scene,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(main_scene_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: self.uniform_buffer.as_entire_binding(),
                },
            ],
        });

        let view_extract = self.view_extract.as_ref().unwrap();
        let view_blur_h = self.view_blur_h.as_ref().unwrap();

        let bg_blur_h = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Bloom Blur H BG"),
            layout: &self.bind_group_layout_scene,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(view_extract),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: self.uniform_buffer.as_entire_binding(),
                },
            ],
        });

        let bg_blur_v = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Bloom Blur V BG"),
            layout: &self.bind_group_layout_scene,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(view_blur_h),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: self.uniform_buffer.as_entire_binding(),
                },
            ],
        });

        let bg_composite_scene = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Bloom Composite Scene BG"),
            layout: &self.bind_group_layout_scene,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(main_scene_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: self.uniform_buffer.as_entire_binding(),
                },
            ],
        });

        let bg_composite_bloom = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Bloom Composite Bloom BG"),
            layout: &self.bind_group_layout_bloom,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(view_extract), // Extract now holds the blurred result
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });

        // 1. Extract
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Bloom Extract Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: view_extract,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::TRANSPARENT),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            pass.set_pipeline(&self.extract_pipeline);
            pass.set_bind_group(0, &bg_extract, &[]);
            pass.draw(0..3, 0..1);
        }

        // 2. Blur H
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Bloom Blur H Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: view_blur_h,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::TRANSPARENT),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            pass.set_pipeline(&self.blur_h_pipeline);
            pass.set_bind_group(0, &bg_blur_h, &[]);
            pass.draw(0..3, 0..1);
        }

        // 3. Blur V
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Bloom Blur V Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: view_extract,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::TRANSPARENT),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            pass.set_pipeline(&self.blur_v_pipeline);
            pass.set_bind_group(0, &bg_blur_v, &[]);
            pass.draw(0..3, 0..1);
        }

        // 4. Composite
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Bloom Composite Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: target_view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::TRANSPARENT),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            pass.set_pipeline(&self.composite_pipeline);
            pass.set_bind_group(0, &bg_composite_scene, &[]);
            pass.set_bind_group(1, &bg_composite_bloom, &[]);
            pass.draw(0..3, 0..1);
        }
    }
}
