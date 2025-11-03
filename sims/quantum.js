/*
Schrödinger Equation in 2D: 

This simulation solves the time-dependent Schrödinger equation for a quantum wave packet interacting with a potential barrier. A Gaussian wave packet is initialized with momentum directed toward the barrier, representing a particle in quantum mechanics. At each timestep, we evolve the complex-valued wave function by computing the Laplacian (kinetic energy term) and applying the potential energy from the barrier. When the wave packet encounters the barrier, it exhibits quantum tunneling and reflection. The visualization uses domain coloring to represent the complex wave function: the phase (angle) is mapped to hue in a color wheel, and the probability density (magnitude squared) determines the brightness and height of the surface.

The simulation itself is very numerically unstable and actually breaks after about 30 seconds or so.
*/




const GRID_SIZE = 120;
const GRID_SPACING = 0.2;
const WORKGROUP_SIZE = 8;
const WORKGROUP_COUNT = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);
const CELL_COUNT = GRID_SIZE * GRID_SIZE;

const DT = 0.001;

const BARRIER_ENABLED = 1;
const BARRIER_X = GRID_SIZE * 0.6;
const BARRIER_WIDTH = 8.0;
const BARRIER_HEIGHT = 50.0;

const vertexShaderCode = `
struct Uniforms {
    mvp: mat4x4<f32>,
};
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> psiReal: array<f32>;
@binding(2) @group(0) var<storage, read> psiImag: array<f32>;

struct VertexInput {
    @location(0) xz: vec2<f32>,
    @builtin(vertex_index) vertexIndex: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) psi: vec2<f32>,
    @location(1) worldPos: vec3<f32>,
};

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

@vertex
fn main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    let idx = input.vertexIndex;
    let real = psiReal[idx];
    let imag = psiImag[idx];

    let prob = real * real + imag * imag;
    let h = prob * 2.0;

    let pos = vec3<f32>(input.xz.x, h, input.xz.y);
    output.position = uniforms.mvp * vec4<f32>(pos, 1.0);
    output.psi = vec2<f32>(real, imag);
    output.worldPos = pos;
    return output;
}
`;

const fragmentShaderCode = `
@fragment
fn main(@location(0) psi: vec2<f32>, @location(1) worldPos: vec3<f32>) -> @location(0) vec4<f32> {

    let phase = atan2(psi.y, psi.x);
    let mag = length(psi);

    let hue = (phase + 3.14159265) / (2.0 * 3.14159265);

    let h6 = hue * 6.0;
    let x = 1.0 - abs((h6 % 2.0) - 1.0);
    var rgb: vec3<f32>;

    if (h6 < 1.0) {
        rgb = vec3<f32>(1.0, x, 0.0);
    } else if (h6 < 2.0) {
        rgb = vec3<f32>(x, 1.0, 0.0);
    } else if (h6 < 3.0) {
        rgb = vec3<f32>(0.0, 1.0, x);
    } else if (h6 < 4.0) {
        rgb = vec3<f32>(0.0, x, 1.0);
    } else if (h6 < 5.0) {
        rgb = vec3<f32>(x, 0.0, 1.0);
    } else {
        rgb = vec3<f32>(1.0, 0.0, x);
    }

    let brightness = 1.0 - exp(-mag * 3.0);
    let color = rgb * brightness;

    return vec4<f32>(color, 1.0);
}
`;

const initShaderCode = `
@binding(0) @group(0) var<storage, read_write> psiReal: array<f32>;
@binding(1) @group(0) var<storage, read_write> psiImag: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let i = cellIndex(cell.x, cell.y);

    let x = f32(cell.x) * ${GRID_SPACING};
    let y = f32(cell.y) * ${GRID_SPACING};

    let x0 = f32(${GRID_SIZE}) * ${GRID_SPACING} * 0.25;
    let y0 = f32(${GRID_SIZE}) * ${GRID_SPACING} * 0.5;
    let sigma = 2.0;
    let kx = 8.0;  
    let ky = 0.0;

    let dx = x - x0;
    let dy = y - y0;
    let r2 = dx * dx + dy * dy;

    let envelope = exp(-r2 / (2.0 * sigma * sigma));
    let phase = kx * dx + ky * dy;

    psiReal[i] = envelope * cos(phase);
    psiImag[i] = envelope * sin(phase);
}
`;

const evolveShaderCode = `
@binding(0) @group(0) var<storage, read> psiRealIn: array<f32>;
@binding(1) @group(0) var<storage, read> psiImagIn: array<f32>;
@binding(2) @group(0) var<storage, read_write> psiRealOut: array<f32>;
@binding(3) @group(0) var<storage, read_write> psiImagOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE}u + x;
}

fn wrapIndex(idx: i32) -> u32 {
    if (idx < 0) {
        return u32(idx + ${GRID_SIZE});
    } else if (idx >= ${GRID_SIZE}) {
        return u32(idx - ${GRID_SIZE});
    }
    return u32(idx);
}

fn potential(x: i32, y: i32) -> f32 {
    if (${BARRIER_ENABLED} == 0) {
        return 0.0;
    }

    let fx = f32(x);
    let dist = abs(fx - ${BARRIER_X});

    if (dist < ${BARRIER_WIDTH}) {
        return ${BARRIER_HEIGHT};
    }
    return 0.0;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) {
        return;
    }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    let real = psiRealIn[i];
    let imag = psiImagIn[i];

    let dx = ${GRID_SPACING};
    let dx2 = dx * dx;

    let real_c = real;
    let real_l = psiRealIn[cellIndex(wrapIndex(x - 1), u32(y))];
    let real_r = psiRealIn[cellIndex(wrapIndex(x + 1), u32(y))];
    let real_d = psiRealIn[cellIndex(u32(x), wrapIndex(y - 1))];
    let real_u = psiRealIn[cellIndex(u32(x), wrapIndex(y + 1))];

    let imag_c = imag;
    let imag_l = psiImagIn[cellIndex(wrapIndex(x - 1), u32(y))];
    let imag_r = psiImagIn[cellIndex(wrapIndex(x + 1), u32(y))];
    let imag_d = psiImagIn[cellIndex(u32(x), wrapIndex(y - 1))];
    let imag_u = psiImagIn[cellIndex(u32(x), wrapIndex(y + 1))];

    let laplacian_real = (real_l + real_r + real_d + real_u - 4.0 * real_c) / dx2;
    let laplacian_imag = (imag_l + imag_r + imag_d + imag_u - 4.0 * imag_c) / dx2;

    let V = potential(x, y);
    let dt = ${DT};

    let d_real_dt = 0.5 * laplacian_imag - V * imag;
    let d_imag_dt = -0.5 * laplacian_real + V * real;

    psiRealOut[i] = real + dt * d_real_dt;
    psiImagOut[i] = imag + dt * d_imag_dt;
}
`;

async function initQuantumSim() {
  const canvas = document.getElementById("canvas");
  const { device, context, format } = await initWebGPU(canvas);

  let depthTexture = createDepthTexture(device);

  const { xzBuffer, indices, indexBuffer } = gridBuffers(
    device,
    GRID_SIZE,
    GRID_SPACING,
  );

  const psiRealBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];

  const psiImagBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];

  const mvpBuffer = createUniformBuffer(device, 64);

  const renderPipeline = create_render_pipeline(
    device,
    vertexShaderCode,
    fragmentShaderCode,
    format,
  );

  const initPipeline = create_compute_pipeline(device, initShaderCode);

  const evolvePipeline = create_compute_pipeline(device, evolveShaderCode);

  submit_commands(device, (encoder) => {
    dispatch(
      device,
      encoder,
      initPipeline,
      [psiRealBuffers[0], psiImagBuffers[0]],
      WORKGROUP_COUNT,
    );
  });

  let step = 0;

  const camera = createOrbitCamera({
    radius: 15.0,
    height: 8.0,
    speed: 0.0002,
  });

  const renderPassConfig = createRenderPassConfig({
    clearColor: { r: 0.05, g: 0.05, b: 0.1, a: 1.0 },
  });

  function render() {
    if (handleCanvasResize(canvas)) {
      depthTexture = recreateTexture(device, canvas, depthTexture);
    }

    const readIdx = step % 2;
    const writeIdx = (step + 1) % 2;

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        evolvePipeline,
        [
          psiRealBuffers[readIdx],
          psiImagBuffers[readIdx],
          psiRealBuffers[writeIdx],
          psiImagBuffers[writeIdx],
        ],
        WORKGROUP_COUNT,
      );
    });

    step++;

    const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
    device.queue.writeBuffer(mvpBuffer, 0, mvp);

    submit_commands(device, (encoder) => {
      const renderPass = beginColorRenderPass(
        encoder,
        context,
        renderPassConfig,
        depthTexture,
      );

      draw(
        device,
        renderPass,
        renderPipeline,
        [mvpBuffer, psiRealBuffers[step % 2], psiImagBuffers[step % 2]],
        xzBuffer,
        indexBuffer,
        indices.length,
      );

      renderPass.end();
    });

    requestAnimationFrame(render);
  }

  render();
}

initQuantumSim();