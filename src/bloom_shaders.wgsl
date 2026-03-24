struct VertexOutput {
    @builtin(position) pos: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@vertex
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> VertexOutput {
    var out: VertexOutput;
    let x = f32(i32(in_vertex_index) / 2) * 4.0 - 1.0;
    let y = f32(i32(in_vertex_index) % 2) * 4.0 - 1.0;
    out.pos = vec4<f32>(x, y, 0.0, 1.0);
    out.uv = vec2<f32>(x * 0.5 + 0.5, 1.0 - (y * 0.5 + 0.5));
    return out;
}

@group(0) @binding(0) var scene_tex: texture_2d<f32>;
@group(0) @binding(1) var scene_samp: sampler;

struct BloomUniforms {
    threshold: f32,
    intensity: f32,
    opacity: f32,
    _pad0: f32,
};
@group(0) @binding(2) var<uniform> uniforms: BloomUniforms;

@fragment
fn fs_extract(in: VertexOutput) -> @location(0) vec4<f32> {
    let color = textureSample(scene_tex, scene_samp, in.uv).rgb;
    let brightness = dot(color, vec3<f32>(0.2126, 0.7152, 0.0722));
    if (brightness > uniforms.threshold) {
        return vec4<f32>(color, 1.0);
    } else {
        return vec4<f32>(0.0, 0.0, 0.0, 1.0);
    }
}

@fragment
fn fs_blur(in: VertexOutput) -> @location(0) vec4<f32> {
    let dims = vec2<f32>(textureDimensions(scene_tex));
    let offset = 1.0 / dims;
    
    // Very tight 5-tap Gaussian
    var result = textureSample(scene_tex, scene_samp, in.uv).rgb * 0.44198;
    result += textureSample(scene_tex, scene_samp, in.uv + vec2<f32>(offset.x, 0.0)).rgb * 0.27901;
    result += textureSample(scene_tex, scene_samp, in.uv - vec2<f32>(offset.x, 0.0)).rgb * 0.27901;
    
    return vec4<f32>(result, 1.0);
}

@fragment
fn fs_blur_v(in: VertexOutput) -> @location(0) vec4<f32> {
    let dims = vec2<f32>(textureDimensions(scene_tex));
    let offset = 1.0 / dims;
    
    var result = textureSample(scene_tex, scene_samp, in.uv).rgb * 0.44198;
    result += textureSample(scene_tex, scene_samp, in.uv + vec2<f32>(0.0, offset.y)).rgb * 0.27901;
    result += textureSample(scene_tex, scene_samp, in.uv - vec2<f32>(0.0, offset.y)).rgb * 0.27901;
    
    return vec4<f32>(result, 1.0);
}

@group(1) @binding(0) var bloom_tex: texture_2d<f32>;
@group(1) @binding(1) var bloom_samp: sampler;

@fragment
fn fs_composite(in: VertexOutput) -> @location(0) vec4<f32> {
    // Use textureLoad for the scene to ensure absolute pixel-per-pixel accuracy
    let coords = vec2<i32>(in.pos.xy);
    let scene_color = textureLoad(scene_tex, coords, 0);
    
    // Bloom can still use sampling for the smooth glow
    let bloom_color = textureSample(bloom_tex, bloom_samp, in.uv);
    
    let result = scene_color.rgb + bloom_color.rgb * uniforms.intensity;
    return vec4<f32>(result, uniforms.opacity);
}
