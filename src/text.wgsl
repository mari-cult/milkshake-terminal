struct VertexInput {
    @location(0) pos: vec2<f32>,
    @location(1) uv: vec2<f32>,
    @location(2) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) pos: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) color: vec4<f32>,
}

@group(0) @binding(0) var atlas_tex: texture_2d<f32>;
@group(0) @binding(1) var atlas_samp: sampler;

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.pos = vec4<f32>(in.pos, 0.0, 1.0);
    out.uv = in.uv;
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let d = textureSample(atlas_tex, atlas_samp, in.uv).r;
    // d is roughly 0.5 at boundary. Smoothstep using derivatives:
    let threshold = 0.51;
    let smoothing = fwidth(d) * 0.7; // slight smoothing based on fragment deriv
    let alpha = smoothstep(threshold - smoothing, threshold + smoothing, d);
    return vec4<f32>(in.color.rgb, in.color.a * alpha);
}
