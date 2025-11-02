const GRID_SIZE = 120;
const GRID_SPACING = 0.2;
const WORKGROUP_SIZE = 8;
const WORKGROUP_COUNT = Math.ceil(GRID_SIZE / WORKGROUP_SIZE);
const CELL_COUNT = GRID_SIZE * GRID_SIZE;

const FORCE_SPAWN_INTERVAL = 100;
const JACOBI_ITERATIONS = 30;
const VELOCITY_DIFFUSION_ITERATIONS = 3;
const DENSITY_DIFFUSION_ITERATIONS = 2;
const VISCOSITY = 0.001;
const DENSITY_DIFFUSION = 0.01;
const DT = 0.02;
const JET_STRENGTH = 200.0;
const JET_WIDTH = 12.0;
const JET_LENGTH = 35.0;
const DENSITY_DECAY = 0.995;

const vertexShaderCode = `
struct Uniforms { mvp: mat4x4<f32> };
@binding(0) @group(0) var<uniform> uniforms: Uniforms;
@binding(1) @group(0) var<storage, read> densityField: array<f32>;

struct VertexInput {
  @location(0) xz: vec2<f32>,
  @builtin(vertex_index) vertexIndex: u32
};

struct VertexOutput {
  @builtin(position) position: vec4<f32>,
  @location(0) density: f32,
  @location(1) distance: f32
};

@vertex
fn main(input: VertexInput) -> VertexOutput {
  var output: VertexOutput;
  let pos = vec3<f32>(input.xz.x, 0.0, input.xz.y);
  output.position = uniforms.mvp * vec4<f32>(pos, 1.0);
  output.density = densityField[input.vertexIndex];
  output.distance = length(pos);
  return output;
}
`;

const fragmentShaderCode = `
@fragment
fn main(@location(0) density: f32, @location(1) distance: f32) -> @location(0) vec4<f32> {
  let d = clamp(density, 0.0, 1.0);
  var color: vec3<f32>;
  if (d < 0.25) {
    let t = d * 4.0;
    color = mix(vec3<f32>(0.0, 0.0, 0.3), vec3<f32>(0.0, 0.5, 1.0), t);
  } else if (d < 0.5) {
    let t = (d - 0.25) * 4.0;
    color = mix(vec3<f32>(0.0, 0.5, 1.0), vec3<f32>(0.0, 1.0, 1.0), t);
  } else if (d < 0.75) {
    let t = (d - 0.5) * 4.0;
    color = mix(vec3<f32>(0.0, 1.0, 1.0), vec3<f32>(1.0, 1.0, 0.0), t);
  } else {
    let t = (d - 0.75) * 4.0;
    color = mix(vec3<f32>(1.0, 1.0, 0.0), vec3<f32>(1.0, 0.0, 0.0), t);
  }
  let fadeStart = 8.0;
  let fadeEnd = 14.0;
  let alpha = 1.0 - smoothstep(fadeStart, fadeEnd, distance);
  return vec4<f32>(color, alpha);
}
`;

const advectShaderCode = `
@binding(0) @group(0) var<storage, read> velocityIn: array<vec2f>;
@binding(1) @group(0) var<storage, read> fieldIn: array<vec2f>;
@binding(2) @group(0) var<storage, read_write> fieldOut: array<vec2f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn sampleBilinear(pos: vec2f) -> vec2f {
    let gridSize = f32(${GRID_SIZE});
    let x = clamp(pos.x, 0.0, gridSize - 1.001);
    let y = clamp(pos.y, 0.0, gridSize - 1.001);

    let x0 = u32(floor(x));
    let y0 = u32(floor(y));
    let x1 = min(x0 + 1u, u32(gridSize - 1.0));
    let y1 = min(y0 + 1u, u32(gridSize - 1.0));

    let fx = fract(x);
    let fy = fract(y);

    let v00 = fieldIn[cellIndex(x0, y0)];
    let v10 = fieldIn[cellIndex(x1, y0)];
    let v01 = fieldIn[cellIndex(x0, y1)];
    let v11 = fieldIn[cellIndex(x1, y1)];

    let v0 = mix(v00, v10, fx);
    let v1 = mix(v01, v11, fx);

    return mix(v0, v1, fy);
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) { return; }

    let i = cellIndex(cell.x, cell.y);
    let dt = f32(${DT});
    let pos = vec2f(f32(cell.x), f32(cell.y));
    let vel = velocityIn[i];
    let prevPos = pos - vel * dt;

    fieldOut[i] = sampleBilinear(prevPos);
}
`;

// Separate velocity diffusion shader (for vec2f)
const velocityDiffuseShaderCode = `
@binding(0) @group(0) var<storage, read> fieldIn: array<vec2f>;
@binding(1) @group(0) var<storage, read_write> fieldOut: array<vec2f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn getVelocity(x: i32, y: i32) -> vec2f {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return vec2f(0.0, 0.0);
    }
    return fieldIn[cellIndex(u32(x), u32(y))];
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) { return; }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    let viscosity = f32(${VISCOSITY});
    let dt = f32(${DT});
    let dx = f32(${GRID_SPACING});
    let a = dt * viscosity / (dx * dx);

    let center = fieldIn[i];
    let left = getVelocity(x - 1, y);
    let right = getVelocity(x + 1, y);
    let bottom = getVelocity(x, y - 1);
    let top = getVelocity(x, y + 1);

    fieldOut[i] = (center + a * (left + right + bottom + top)) / (1.0 + 4.0 * a);
}
`;

// New density diffusion shader (for f32)
const densityDiffuseShaderCode = `
@binding(0) @group(0) var<storage, read> densityIn: array<f32>;
@binding(1) @group(0) var<storage, read_write> densityOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn getDensity(x: i32, y: i32) -> f32 {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return 0.0;
    }
    return densityIn[cellIndex(u32(x), u32(y))];
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) { return; }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    let diffusion = f32(${DENSITY_DIFFUSION});
    let dt = f32(${DT});
    let dx = f32(${GRID_SPACING});
    let a = dt * diffusion / (dx * dx);

    let center = densityIn[i];
    let left = getDensity(x - 1, y);
    let right = getDensity(x + 1, y);
    let bottom = getDensity(x, y - 1);
    let top = getDensity(x, y + 1);

    densityOut[i] = (center + a * (left + right + bottom + top)) / (1.0 + 4.0 * a);
}
`;

const divergenceShaderCode = `
@binding(0) @group(0) var<storage, read> velocityField: array<vec2f>;
@binding(1) @group(0) var<storage, read_write> divergence: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn getVelocity(x: i32, y: i32) -> vec2f {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) {
        return vec2f(0.0, 0.0);
    }
    return velocityField[cellIndex(u32(x), u32(y))];
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) { return; }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    let left = getVelocity(x - 1, y).x;
    let right = getVelocity(x + 1, y).x;
    let bottom = getVelocity(x, y - 1).y;
    let top = getVelocity(x, y + 1).y;

    let dx = f32(${GRID_SPACING});
    divergence[i] = 0.5 * ((right - left) + (top - bottom)) / dx;
}
`;

const pressureShaderCode = `
@binding(0) @group(0) var<storage, read> divergence: array<f32>;
@binding(1) @group(0) var<storage, read> pressureIn: array<f32>;
@binding(2) @group(0) var<storage, read_write> pressureOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn getPressure(x: i32, y: i32) -> f32 {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) { return 0.0; }
    return pressureIn[cellIndex(u32(x), u32(y))];
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) { return; }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    let left = getPressure(x - 1, y);
    let right = getPressure(x + 1, y);
    let bottom = getPressure(x, y - 1);
    let top = getPressure(x, y + 1);

    let dx = f32(${GRID_SPACING});

    pressureOut[i] = (left + right + bottom + top - divergence[i] * dx * dx) * 0.25;
}
`;

const gradientSubtractShaderCode = `
@binding(0) @group(0) var<storage, read> pressure: array<f32>;
@binding(1) @group(0) var<storage, read> velocityIn: array<vec2f>;
@binding(2) @group(0) var<storage, read_write> velocityOut: array<vec2f>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn getPressure(x: i32, y: i32) -> f32 {
    if (x < 0 || x >= ${GRID_SIZE} || y < 0 || y >= ${GRID_SIZE}) { return 0.0; }
    return pressure[cellIndex(u32(x), u32(y))];
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) { return; }

    let x = i32(cell.x);
    let y = i32(cell.y);
    let i = cellIndex(cell.x, cell.y);

    let left = getPressure(x - 1, y);
    let right = getPressure(x + 1, y);
    let bottom = getPressure(x, y - 1);
    let top = getPressure(x, y + 1);

    let dx = f32(${GRID_SPACING});
    let gradP = vec2f(right - left, top - bottom) / (2.0 * dx);
    velocityOut[i] = velocityIn[i] - gradP;
}
`;

const densityAdvectShaderCode = `
@binding(0) @group(0) var<storage, read> velocityField: array<vec2f>;
@binding(1) @group(0) var<storage, read> densityIn: array<f32>;
@binding(2) @group(0) var<storage, read_write> densityOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

fn sampleDensity(pos: vec2f) -> f32 {
    let gridSize = f32(${GRID_SIZE});
    let x = clamp(pos.x, 0.0, gridSize - 1.001);
    let y = clamp(pos.y, 0.0, gridSize - 1.001);

    let x0 = u32(floor(x));
    let y0 = u32(floor(y));
    let x1 = min(x0 + 1u, u32(gridSize - 1.0));
    let y1 = min(y0 + 1u, u32(gridSize - 1.0));

    let fx = fract(x);
    let fy = fract(y);

    let d00 = densityIn[cellIndex(x0, y0)];
    let d10 = densityIn[cellIndex(x1, y0)];
    let d01 = densityIn[cellIndex(x0, y1)];
    let d11 = densityIn[cellIndex(x1, y1)];

    let d0 = mix(d00, d10, fx);
    let d1 = mix(d01, d11, fx);

    return mix(d0, d1, fy);
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) { return; }

    let i = cellIndex(cell.x, cell.y);
    let dt = f32(${DT});
    let pos = vec2f(f32(cell.x), f32(cell.y));
    let vel = velocityField[i];
    let prevPos = pos - vel * dt;

    // Apply decay for dissipation
    densityOut[i] = sampleDensity(prevPos) * f32(${DENSITY_DECAY});
}
`;

const addJetShaderCode = `
@binding(0) @group(0) var<uniform> jetParams: vec4f;
@binding(1) @group(0) var<storage, read> velocityIn: array<vec2f>;
@binding(2) @group(0) var<storage, read_write> velocityOut: array<vec2f>;
@binding(3) @group(0) var<storage, read> densityIn: array<f32>;
@binding(4) @group(0) var<storage, read_write> densityOut: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * ${GRID_SIZE} + x;
}

@compute @workgroup_size(${WORKGROUP_SIZE}, ${WORKGROUP_SIZE})
fn main(@builtin(global_invocation_id) cell: vec3u) {
    if (cell.x >= ${GRID_SIZE}u || cell.y >= ${GRID_SIZE}u) { return; }

    let i = cellIndex(cell.x, cell.y);
    let pos = vec2f(f32(cell.x), f32(cell.y));
    let sourcePos = jetParams.xy;
    let angle = jetParams.z;
    let strength = jetParams.w;
    
    let jetDir = vec2f(cos(angle), sin(angle));
    let toSource = pos - sourcePos;
    
    let perpDist = abs(toSource.x * jetDir.y - toSource.y * jetDir.x);
    let alongDist = toSource.x * jetDir.x + toSource.y * jetDir.y;
    
    let jetWidth = f32(${JET_WIDTH});
    let jetLength = f32(${JET_LENGTH});
    
    if (perpDist < jetWidth && alongDist > 0.0 && alongDist < jetLength) {
        let widthFalloff = 1.0 - (perpDist / jetWidth);
        let lengthFalloff = 1.0 - (alongDist / jetLength);
        let force = jetDir * strength * widthFalloff * lengthFalloff;
        
        velocityOut[i] = velocityIn[i] + force;
        densityOut[i] = min(densityIn[i] + widthFalloff * lengthFalloff * 0.5, 1.0);
    } else {
        velocityOut[i] = velocityIn[i];
        densityOut[i] = densityIn[i];
    }
}
`;

async function initFluidSim() {
  const canvas = document.getElementById("canvas");
  const { device, context, format } = await initWebGPU(canvas);
  const { xzBuffer, indices, indexBuffer } = gridBuffers(
    device,
    GRID_SIZE,
    GRID_SPACING,
  );
  let depthTexture = createDepthTexture(device, canvas);

  const velocityBuffers = [
    createStorageBuffer(device, CELL_COUNT * 8),
    createStorageBuffer(device, CELL_COUNT * 8),
  ];
  const densityBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];
  const pressureBuffers = [
    createStorageBuffer(device, CELL_COUNT * 4),
    createStorageBuffer(device, CELL_COUNT * 4),
  ];
  const divergenceBuffer = createStorageBuffer(device, CELL_COUNT * 4);



  const mvpBuffer = createUniformBuffer(device, 64);
  const jetParamsBuffer = createUniformBuffer(device, 16);

  const renderPipeline = create_render_pipeline(
    device,
    vertexShaderCode,
    fragmentShaderCode,
    format,
  );
  const advectPipeline = create_compute_pipeline(device, advectShaderCode);
  const velocityDiffusePipeline = create_compute_pipeline(device, velocityDiffuseShaderCode);
  const densityDiffusePipeline = create_compute_pipeline(device, densityDiffuseShaderCode);
  const divergencePipeline = create_compute_pipeline(
    device,
    divergenceShaderCode,
  );
  const pressurePipeline = create_compute_pipeline(device, pressureShaderCode);
  const gradientSubtractPipeline = create_compute_pipeline(
    device,
    gradientSubtractShaderCode,
  );
  const densityAdvectPipeline = create_compute_pipeline(
    device,
    densityAdvectShaderCode,
  );
  const addJetPipeline = create_compute_pipeline(device, addJetShaderCode);

  let step = 0;
  let frameCount = 0;

  function addJet(posX, posY, angle, strength) {
    device.queue.writeBuffer(
      jetParamsBuffer,
      0,
      new Float32Array([posX, posY, angle, strength]),
    );
    const src = step % 2;
    let dst = (step + 1) % 2;

    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        addJetPipeline,
        [
          jetParamsBuffer,
          velocityBuffers[src],
          velocityBuffers[dst],
          densityBuffers[src],
          densityBuffers[dst],
        ],
        WORKGROUP_COUNT,
      );
    });
    step = dst;
  }

  function addJetPair() {
    const separation = 30.0;
    const centerY = GRID_SIZE * 0.5;
    const y1 = centerY - separation * 0.5;
    const y2 = centerY + separation * 0.5;
    const x = GRID_SIZE * 0.2;

    const angle1 = Math.PI * 0.1;
    const angle2 = -Math.PI * 0.1;

    addJet(x, y1, angle1, JET_STRENGTH);
    addJet(x, y2, angle2, JET_STRENGTH);
  }

  function simulateStep() {
    const src = step % 2;
    const dst = (step + 1) % 2;

    // Step 1: Advect velocity
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        advectPipeline,
        [velocityBuffers[src], velocityBuffers[src], velocityBuffers[dst]],
        WORKGROUP_COUNT,
      );
    });

    // Step 2: Diffuse velocity (reduced iterations)
    for (let iter = 0; iter < VELOCITY_DIFFUSION_ITERATIONS; iter++) {
      const diffSrc = iter === 0 ? dst : (dst + iter) % 2;
      const diffDst = (dst + iter + 1) % 2;

      submit_commands(device, (encoder) => {
        dispatch(
          device,
          encoder,
          velocityDiffusePipeline,
          [velocityBuffers[diffSrc], velocityBuffers[diffDst]],
          WORKGROUP_COUNT,
        );
      });
    }

    const finalDiffusedIdx = (dst + VELOCITY_DIFFUSION_ITERATIONS) % 2;

    // Step 3: Project (make incompressible)
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        divergencePipeline,
        [velocityBuffers[finalDiffusedIdx], divergenceBuffer],
        WORKGROUP_COUNT,
      );
    });

    // Reset pressure buffers
    device.queue.writeBuffer(
      pressureBuffers[0],
      0,
      new Float32Array(CELL_COUNT),
    );
    device.queue.writeBuffer(
      pressureBuffers[1],
      0,
      new Float32Array(CELL_COUNT),
    );

    // Jacobi iterations for pressure
    for (let iter = 0; iter < JACOBI_ITERATIONS; iter++) {
      submit_commands(device, (encoder) => {
        dispatch(
          device,
          encoder,
          pressurePipeline,
          [
            divergenceBuffer,
            pressureBuffers[iter % 2],
            pressureBuffers[(iter + 1) % 2],
          ],
          WORKGROUP_COUNT,
        );
      });
    }

    // Subtract pressure gradient
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        gradientSubtractPipeline,
        [
          pressureBuffers[JACOBI_ITERATIONS % 2],
          velocityBuffers[finalDiffusedIdx],
          velocityBuffers[dst],
        ],
        WORKGROUP_COUNT,
      );
    });

    // Step 4: Advect density
    submit_commands(device, (encoder) => {
      dispatch(
        device,
        encoder,
        densityAdvectPipeline,
        [velocityBuffers[dst], densityBuffers[src], densityBuffers[dst]],
        WORKGROUP_COUNT,
      );
    });

    // Step 5: Diffuse density (optional, for smoother smoke)
    for (let iter = 0; iter < DENSITY_DIFFUSION_ITERATIONS; iter++) {
      const diffSrc = iter === 0 ? dst : (dst + iter) % 2;
      const diffDst = (dst + iter + 1) % 2;

      submit_commands(device, (encoder) => {
        dispatch(
          device,
          encoder,
          densityDiffusePipeline,
          [densityBuffers[diffSrc], densityBuffers[diffDst]],
          WORKGROUP_COUNT,
        );
      });
    }

    step = (dst + DENSITY_DIFFUSION_ITERATIONS) % 2;
  }

  const camera = createOrbitCamera({
    radius: 0.0,
    height: 15.0,
    speed: 0.0003,
  });

  const renderPassConfig = createRenderPassConfig({
    clearColor: { r: 0.0, g: 0.0, b: 0.0, a: 1.0 },
  });

  addJetPair();

  function render() {
    if (handleCanvasResize(canvas)) {
      depthTexture = recreateTexture(device, canvas, depthTexture);
    }

    const mvp = updateOrbitCamera(camera, Date.now(), getAspectRatio(canvas));
    device.queue.writeBuffer(mvpBuffer, 0, mvp);

    frameCount++;

    if (frameCount % FORCE_SPAWN_INTERVAL === 0) {
      addJetPair();
    }

    simulateStep();

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
        [mvpBuffer, densityBuffers[step % 2]],
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

initFluidSim();