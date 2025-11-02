const GRID_SIZE = 120;
const GRID_SPACING = 0.2;
const WORKGROUP_SIZE = 8;
const WORKGROUP_COUNT = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);
const CELL_COUNT = GRID_SIZE * GRID_SIZE;
const DT = 0.03;
const NUM_PARTICLES = 8;

const heightGenShaderCode = `
@binding(0) @group(0) var<storage, read_write> height: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

fn compute_height(x: u32, y: u32) -> f32 {
    let world_x = (f32(x) - ${GRID_SIZE}.0/2.0) * ${GRID_SPACING};
    let world_y = (f32(y) - ${GRID_SIZE}.0/2.0) * ${GRID_SPACING};

    let r2 = world_x*world_x + world_y*world_y;
    let bump = -10.0 * exp(-r2 / 4.0);

    let dx = world_x - 5.0;
    let dy = world_y - 3.0;
    let saddle = 0.5 * (dx*dx - dy*dy) * exp(-(dx*dx + dy*dy)/3.0);

    return bump + saddle;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let idx = cellIndex(cell.x, cell.y);
    height[idx] = compute_height(cell.x, cell.y);
}
`;

const metricComputeShaderCode = `
@binding(0) @group(0) var<storage, read> height: array<f32>;
@binding(1) @group(0) var<storage, read_write> metric: array<vec3f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

fn get_height(x: i32, y: i32) -> f32 {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return 0.0;
    }
    return height[cellIndex(u32(x), u32(y))];
}

fn compute_derivatives(x: i32, y: i32) -> vec2f {
    let dx = ${GRID_SPACING};
    let h_x = (get_height(x+1, y) - get_height(x-1, y)) / (2.0 * dx);
    let h_y = (get_height(x, y+1) - get_height(x, y-1)) / (2.0 * dx);
    return vec2f(h_x, h_y);
}

fn compute_metric(h_x: f32, h_y: f32) -> vec3f {
    let x_step = 1.0f;
    let g_xx = (x_step * x_step) + (h_x * h_x);
    let g_yy = 1.0 + h_y * h_y;
    let g_xy = h_x * h_y;
    return vec3f(g_xx, g_xy, g_yy);
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let idx = cellIndex(cell.x, cell.y);

    let derivs = compute_derivatives(x, y);
    let g = compute_metric(derivs.x, derivs.y);

    metric[idx] = g;
}
`;

const christoffelComputeShaderCode = `
@binding(0) @group(0) var<storage, read> height: array<f32>;
@binding(1) @group(0) var<storage, read> metric: array<vec3f>;
@binding(2) @group(0) var<storage, read_write> christoffel: array<vec3f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

fn get_height(x: i32, y: i32) -> f32 {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return 0.0;
    }
    return height[cellIndex(u32(x), u32(y))];
}

fn compute_height_derivatives(x: i32, y: i32) -> mat2x3<f32> {
    let dx = ${GRID_SPACING};

    let h_x = (get_height(x+1, y) - get_height(x-1, y)) / (2.0 * dx);
    let h_y = (get_height(x, y+1) - get_height(x, y-1)) / (2.0 * dx);

    let h_c = get_height(x, y);
    let h_xx = (get_height(x+1, y) - 2.0*h_c + get_height(x-1, y)) / (dx * dx);
    let h_yy = (get_height(x, y+1) - 2.0*h_c + get_height(x, y-1)) / (dx * dx);

    let h_xy = (get_height(x+1, y+1) - get_height(x+1, y-1)
                - get_height(x-1, y+1) + get_height(x-1, y-1)) / (4.0 * dx * dx);

    return mat2x3<f32>(
        h_x, h_y, 0.0,
        h_xx, h_xy, h_yy
    );
}

fn compute_metric_derivatives(h_deriv: mat2x3<f32>) -> mat3x3<f32> {
    let h_x = h_deriv[0][0];
    let h_y = h_deriv[0][1];
    let h_xx = h_deriv[1][0];
    let h_xy = h_deriv[1][1];
    let h_yy = h_deriv[1][2];

    let dg_xx_dx = 2.0 * h_x * h_xx;
    let dg_xy_dx = h_xx * h_y + h_x * h_xy;
    let dg_yy_dx = 2.0 * h_y * h_xy;

    let dg_xx_dy = 2.0 * h_x * h_xy;
    let dg_xy_dy = h_xy * h_y + h_x * h_yy;
    let dg_yy_dy = 2.0 * h_y * h_yy;

    return mat3x3<f32>(
        dg_xx_dx, dg_xy_dx, dg_yy_dx,
        dg_xx_dy, dg_xy_dy, dg_yy_dy,
        0.0, 0.0, 0.0
    );
}

fn invert_metric_2x2(g: vec3f) -> vec3f {
    let det = g.x * g.z - g.y * g.y;
    return vec3f(g.z / det, -g.y / det, g.x / det);
}

fn compute_christoffel_symbols(g: vec3f, g_inv: vec3f, dg: mat3x3<f32>) -> vec3f {
    let dg_xx_dx = dg[0][0];
    let dg_xy_dx = dg[0][1];
    let dg_yy_dx = dg[0][2];

    let dg_xx_dy = dg[1][0];
    let dg_xy_dy = dg[1][1];
    let dg_yy_dy = dg[1][2];

    let Gamma_x_xx = 0.5 * g_inv.x * dg_xx_dx + 0.5 * g_inv.y * (2.0 * dg_xy_dx - dg_xx_dy);
    let Gamma_x_xy = 0.5 * g_inv.x * dg_xx_dy + 0.5 * g_inv.y * dg_yy_dx;
    let Gamma_x_yy = 0.5 * g_inv.x * (2.0 * dg_xy_dy - dg_yy_dx) + 0.5 * g_inv.y * dg_yy_dy;

    return vec3f(Gamma_x_xx, Gamma_x_xy, Gamma_x_yy);
}

fn compute_christoffel_symbols_y(g: vec3f, g_inv: vec3f, dg: mat3x3<f32>) -> vec3f {
    let dg_xx_dx = dg[0][0];
    let dg_xy_dx = dg[0][1];
    let dg_yy_dx = dg[0][2];

    let dg_xx_dy = dg[1][0];
    let dg_xy_dy = dg[1][1];
    let dg_yy_dy = dg[1][2];

    let Gamma_y_xx = 0.5 * g_inv.y * dg_xx_dx + 0.5 * g_inv.z * (2.0 * dg_xy_dx - dg_xx_dy);
    let Gamma_y_xy = 0.5 * g_inv.y * dg_xx_dy + 0.5 * g_inv.z * dg_yy_dx;
    let Gamma_y_yy = 0.5 * g_inv.y * (2.0 * dg_xy_dy - dg_yy_dx) + 0.5 * g_inv.z * dg_yy_dy;

    return vec3f(Gamma_y_xx, Gamma_y_xy, Gamma_y_yy);
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let idx = cellIndex(cell.x, cell.y);

    let g = metric[idx];
    let g_inv = invert_metric_2x2(g);
    let h_deriv = compute_height_derivatives(x, y);
    let dg = compute_metric_derivatives(h_deriv);

    let gamma_x = compute_christoffel_symbols(g, g_inv, dg);
    let gamma_y = compute_christoffel_symbols_y(g, g_inv, dg);

    christoffel[idx * 2u] = gamma_x;
    christoffel[idx * 2u + 1u] = gamma_y;
}
`;

const particleComputeShaderCode = `
struct Particle {
    pos: vec2f,
    vel: vec2f
};

@binding(0) @group(0) var<storage, read> christoffel: array<vec3f>;
@binding(1) @group(0) var<storage, read_write> particles: array<Particle>;
@binding(2) @group(0) var<uniform> dt: f32;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

fn worldToGrid(p: vec2f) -> vec2f {
    let half = f32(${GRID_SIZE}) * 0.5;
    let gx = p.x / ${GRID_SPACING} + half;
    let gy = p.y / ${GRID_SPACING} + half;
    return clamp(vec2f(gx, gy), vec2f(1.0, 1.0), vec2f(f32(${GRID_SIZE})-2.0, f32(${GRID_SIZE})-2.0));
}

fn idx(i: u32, j: u32) -> u32 {
    return j * ${GRID_SIZE}u + i;
}

fn sampleChristoffel(p: vec2f) -> mat2x3<f32> {
    let gp = worldToGrid(p);
    let ix = u32(floor(gp.x));
    let iy = u32(floor(gp.y));
    let fx = fract(gp.x);
    let fy = fract(gp.y);

    let i00 = idx(ix, iy);
    let i10 = idx(ix+1u, iy);
    let i01 = idx(ix, iy+1u);
    let i11 = idx(ix+1u, iy+1u);

    let gx00 = christoffel[i00*2u];
    let gy00 = christoffel[i00*2u+1u];
    let gx10 = christoffel[i10*2u];
    let gy10 = christoffel[i10*2u+1u];
    let gx01 = christoffel[i01*2u];
    let gy01 = christoffel[i01*2u+1u];
    let gx11 = christoffel[i11*2u];
    let gy11 = christoffel[i11*2u+1u];

    let w00 = (1.0 - fx) * (1.0 - fy);
    let w10 = fx * (1.0 - fy);
    let w01 = (1.0 - fx) * fy;
    let w11 = fx * fy;

    let gx = gx00*w00 + gx10*w10 + gx01*w01 + gx11*w11;
    let gy = gy00*w00 + gy10*w10 + gy01*w01 + gy11*w11;

    return mat2x3<f32>(gx, gy);
}

@compute @workgroup_size(1)
fn main(@builtin(global_invocation_id) id: vec3u) {
    if (id.x >= ${NUM_PARTICLES}u) {
        return;
    }

    var p = particles[id.x];

    let G = sampleChristoffel(p.pos);
    let vx = p.vel.x;
    let vy = p.vel.y;

    let gamma_x = G[0];
    let gamma_y = G[1];

    let ax = - (gamma_x.x * vx*vx + 2.0*gamma_x.y * vx*vy + gamma_x.z * vy*vy);
    let ay = - (gamma_y.x * vx*vx + 2.0*gamma_y.y * vx*vy + gamma_y.z * vy*vy);

    p.vel += vec2f(ax, ay) * dt;
    p.pos += (p.vel + 0.5 * vec2f(ax, ay) * dt) * dt;

    let halfW = f32(${GRID_SIZE}) * 0.5 * ${GRID_SPACING};
    if (abs(p.pos.x) > halfW) {
        p.pos.x = clamp(p.pos.x, -halfW, halfW);
        p.vel.x = -p.vel.x;
    }
    if (abs(p.pos.y) > halfW) {
        p.pos.y = clamp(p.pos.y, -halfW, halfW);
        p.vel.y = -p.vel.y;
    }

    particles[id.x] = p;
}
`;

const vertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> height: array<f32>;
@binding(2) @group(0) var<storage, read> metric: array<vec3f>;
@binding(3) @group(0) var<storage, read> christoffel: array<vec3f>;

struct VertexInput {
    @location(0) xz: vec2<f32>,
    @builtin(vertex_index) vertexIndex: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) christoffel_magnitude: f32,
    @location(1) distance: f32,
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;

    let h = height[input.vertexIndex];
    let gamma_x = christoffel[input.vertexIndex * 2u];
    let gamma_y = christoffel[input.vertexIndex * 2u + 1u];

    let pos = vec3<f32>(input.xz.x, h, input.xz.y);
    output.position = uniforms.mvp * vec4<f32>(pos, 1.0);

    let mag = sqrt(
        gamma_x.x*gamma_x.x + gamma_x.y*gamma_x.y + gamma_x.z*gamma_x.z +
        gamma_y.x*gamma_y.x + gamma_y.y*gamma_y.y + gamma_y.z*gamma_y.z
    );

    output.christoffel_magnitude = mag;
    output.distance = length(pos);

    return output;
}
`;

const fragmentShaderCode = `
@fragment
fn main(@location(0) christoffel_magnitude: f32, @location(1) distance: f32) -> @location(0) vec4<f32> {
    let t = clamp(christoffel_magnitude * 2.0, 0.0, 1.0);
    let color = mix(vec3<f32>(0.2, 0.2, 0.3), vec3<f32>(1.0, 0.5, 0.2), t);

    let fadeStart = 14.0;
    let fadeEnd = 14.0;
    let alpha = 1.0 - smoothstep(fadeStart, fadeEnd, distance);

    return vec4<f32>(color, alpha);
}
`;

const particleVertexShaderCode = `
const SPHERE_RADIUS = 0.1;

struct Uniforms {
    mvp: mat4x4<f32>,
};

struct Particle {
    pos: vec2f,
    vel: vec2f,
};

@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> particles: array<Particle>;
@binding(2) @group(0) var<storage, read> height: array<f32>;
@binding(3) @group(0) var<uniform> particleIndex: u32;

struct VertexInput {
    @location(0) position: vec3f,
    @location(1) normal: vec3f,
};

struct VertexOutput {
    @builtin(position) position: vec4f,
    @location(0) normal: vec3f,
};

fn idx(i:u32, j:u32) -> u32 {
    return j * ${GRID_SIZE}u + i;
}

fn sampleHeight(p: vec2f) -> f32 {
    let half = f32(${GRID_SIZE}) * 0.5;
    let gx = p.x / ${GRID_SPACING} + half;
    let gy = p.y / ${GRID_SPACING} + half;
    let ix = u32(clamp(floor(gx), 0.0, f32(${GRID_SIZE}-2)));
    let iy = u32(clamp(floor(gy), 0.0, f32(${GRID_SIZE}-2)));
    let fx = fract(gx);
    let fy = fract(gy);

    let h00 = height[idx(ix,iy)];
    let h10 = height[idx(ix+1u,iy)];
    let h01 = height[idx(ix,iy+1u)];
    let h11 = height[idx(ix+1u,iy+1u)];

    let h0 = mix(h00, h10, fx);
    let h1 = mix(h01, h11, fx);
    return mix(h0, h1, fy);
}

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;

    let particle = particles[particleIndex];
    let h = sampleHeight(particle.pos);
    let sphereCenter = vec3f(particle.pos.x, h + 0.1, particle.pos.y);
    let worldPos = sphereCenter + input.position * SPHERE_RADIUS;

    output.position = uniforms.mvp * vec4f(worldPos, 1.0);
    output.normal = input.normal;

    return output;
}
`;

const particleFragmentShaderCode = `
@fragment
fn main(@location(0) normal: vec3f) -> @location(0) vec4f {
    let n = normalize(normal);
    let lightDir = normalize(vec3f(0.3, 1.0, 0.4));
    let diffuse = max(dot(n, lightDir), 0.0);
    let ambient = 0.3;
    let color = vec3f(1.0, 0.3, 0.1) * (ambient + diffuse * 0.7);
    return vec4f(color, 1.0);
}
`;

async function initMetricVisualization() {
  const canvas = document.getElementById("canvas");
  const { device, context, format } = await initWebGPU(canvas);

  let depthTexture = createDepthTexture(device);

  const xzPositions = generateGridXZ(GRID_SIZE, GRID_SPACING);
  const xzBuffer = createVertexBuffer(device, xzPositions);
  const indices = generateGridTriangleIndices(GRID_SIZE);
  const indexBuffer = createIndexBuffer(device, indices);

  const heightField = createStorageBuffer(device, CELL_COUNT * 4);
  const metricField = createStorageBuffer(device, CELL_COUNT * 12);
  const christoffelField = createStorageBuffer(device, CELL_COUNT * 24);

  const particlesBuffer = createStorageBuffer(device, NUM_PARTICLES * 16);
  const dtBuffer = createUniformBuffer(device, 4, new Float32Array([DT]));

  const initialParticles = new Float32Array(NUM_PARTICLES * 4);
  for (let i = 0; i < NUM_PARTICLES; i++) {
    const angle = (i / NUM_PARTICLES) * Math.PI * 2;
    const r = 2.0 + (i % 3) * 1.5;
    const x0 = r * Math.cos(angle);
    const y0 = r * Math.sin(angle);

    const speed = 2.0 - (i % 3) * 0.3;
    const vx0 = -speed * Math.sin(angle);
    const vy0 = speed * Math.cos(angle);

    initialParticles[i * 4 + 0] = x0;
    initialParticles[i * 4 + 1] = y0;
    initialParticles[i * 4 + 2] = vx0;
    initialParticles[i * 4 + 3] = vy0;
  }

  device.queue.writeBuffer(particlesBuffer, 0, initialParticles);

  const mvpBuffer = createUniformBuffer(device, 64);

  const particleIndexBuffers = [];
  for (let i = 0; i < NUM_PARTICLES; i++) {
    particleIndexBuffers.push(
      createUniformBuffer(device, 4, new Uint32Array([i])),
    );
  }

  const heightGenPipeline = create_compute_pipeline(
    device,
    heightGenShaderCode,
  );
  const metricComputePipeline = create_compute_pipeline(
    device,
    metricComputeShaderCode,
  );
  const christoffelComputePipeline = create_compute_pipeline(
    device,
    christoffelComputeShaderCode,
  );
  const particleComputePipeline = create_compute_pipeline(
    device,
    particleComputeShaderCode,
  );

  const renderPipeline = create_render_pipeline(
    device,
    vertexShaderCode,
    fragmentShaderCode,
    format,
  );

  const sphere = generateSphere(16);
  const sphereVertexBuffer = createVertexBuffer(device, sphere.positions);
  const sphereNormalBuffer = createVertexBuffer(device, sphere.normals);
  const sphereIndexBuffer = createIndexBuffer(device, sphere.indices);

  const particleRenderPipeline = create_render_pipeline(
    device,
    particleVertexShaderCode,
    particleFragmentShaderCode,
    format,
  );

  function initializeGeometry() {
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        heightGenPipeline,
        [heightField],
        WORKGROUP_COUNT,
      );
    });

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        metricComputePipeline,
        [heightField, metricField],
        WORKGROUP_COUNT,
      );
    });

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        christoffelComputePipeline,
        [heightField, metricField, christoffelField],
        WORKGROUP_COUNT,
      );
    });
  }

  initializeGeometry();

  const camera = createOrbitCamera({
    radius: 0.0,
    height: 10,
    speed: 0.0003,
  });

  const renderPassConfig = createRenderPassConfig({
    clearColor: { r: 0.1, g: 0.1, b: 0.15, a: 1.0 },
  });

  function updateParticles() {
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        particleComputePipeline,
        [christoffelField, particlesBuffer, dtBuffer],
        NUM_PARTICLES,
      );
    });
  }

  function render() {
    if (handleCanvasResize(canvas)) {
      depthTexture = recreateTexture(device, canvas, depthTexture);
    }

    updateParticles();

    const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
    device.queue.writeBuffer(mvpBuffer, 0, mvp);

    submit_commands(device, (encoder) => {
      const pass = beginColorRenderPass(
        encoder,
        context,
        renderPassConfig,
        depthTexture,
      );

      draw(
        device,
        pass,
        renderPipeline,
        [mvpBuffer, heightField, metricField, christoffelField],
        xzBuffer,
        indexBuffer,
        indices.length,
      );

      for (let i = 0; i < NUM_PARTICLES; i++) {
        draw(
          device,
          pass,
          particleRenderPipeline,
          [mvpBuffer, particlesBuffer, heightField, particleIndexBuffers[i]],
          [sphereVertexBuffer, sphereNormalBuffer],
          sphereIndexBuffer,
          sphere.indices.length,
        );
      }

      pass.end();
    });

    requestAnimationFrame(render);
  }

  render();
}

initMetricVisualization();