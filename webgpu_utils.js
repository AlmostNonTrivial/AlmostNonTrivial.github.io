function gridBuffers(device, size, spacing) {
  const xzPositions = generateGridXZ(size, spacing);
  const xzBuffer = createVertexBuffer(device, xzPositions);
  const indices = generateGridTriangleIndices(size);

  const indexBuffer = createIndexBuffer(device, indices);

  return {
    xzPositions,
    xzBuffer,
    indices,
    indexBuffer,
  };
}

function writeBuffer(device, buffer, data) {
  device.queue.writeBuffer(buffer, 0, data);
}

function createUniformBuffer(device, data_or_size, to_write) {
  let buffer;
  if (typeof data_or_size === "number") {
    buffer = device.createBuffer({
      size: data_or_size,
      usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    });
    if (to_write) {
      device.queue.writeBuffer(buffer, 0, to_write);
    }
  } else {
    buffer = device.createBuffer({
      size: data_or_size.byteLength,
      usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    });
    device.queue.writeBuffer(buffer, 0, data_or_size);
  }
  return buffer;
}

function createStorageBuffer(device, size) {
  return device.createBuffer({
    size: size,
    usage:
      GPUBufferUsage.STORAGE |
      GPUBufferUsage.COPY_DST |
      GPUBufferUsage.COPY_SRC,
  });
}

function createVertexBuffer(device, data) {
  const buffer = device.createBuffer({
    size: data.byteLength,
    usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
  });
  device.queue.writeBuffer(buffer, 0, data);
  return buffer;
}

function createIndexBuffer(device, data) {
  const buffer = device.createBuffer({
    size: data.byteLength,
    usage: GPUBufferUsage.INDEX | GPUBufferUsage.COPY_DST,
  });
  device.queue.writeBuffer(buffer, 0, data);
  return buffer;
}

async function initWebGPU(canvas) {
  if (!navigator.gpu) throw new Error("WebGPU not supported");
  const adapter = await navigator.gpu.requestAdapter();
  if (!adapter) throw new Error("No adapter found");
  const device = await adapter.requestDevice();
  const context = canvas.getContext("webgpu");
  const format = navigator.gpu.getPreferredCanvasFormat();
  context.configure({
    device: device,
    format: format,
    alphaMode: "opaque",
  });
  return { device, context, format };
}

function createPerspective(fov, aspect, near, far) {
  const f = 1.0 / Math.tan(fov / 2);
  const nf = 1 / (near - far);
  return new Float32Array([
    f / aspect,
    0,
    0,
    0,
    0,
    f,
    0,
    0,
    0,
    0,
    far * nf,
    -1,
    0,
    0,
    far * near * nf,
    0,
  ]);
}

function createLookAt(eye, target, up) {
  const zAxis = normalize(subtract(eye, target));
  const xAxis = normalize(cross(up, zAxis));
  const yAxis = cross(zAxis, xAxis);
  return new Float32Array([
    xAxis[0],
    yAxis[0],
    zAxis[0],
    0,
    xAxis[1],
    yAxis[1],
    zAxis[1],
    0,
    xAxis[2],
    yAxis[2],
    zAxis[2],
    0,
    -dot(xAxis, eye),
    -dot(yAxis, eye),
    -dot(zAxis, eye),
    1,
  ]);
}

function multiply(a, b) {
  const result = new Float32Array(16);
  for (let row = 0; row < 4; row++) {
    for (let col = 0; col < 4; col++) {
      result[col * 4 + row] =
        a[0 * 4 + row] * b[col * 4 + 0] +
        a[1 * 4 + row] * b[col * 4 + 1] +
        a[2 * 4 + row] * b[col * 4 + 2] +
        a[3 * 4 + row] * b[col * 4 + 3];
    }
  }
  return result;
}

function subtract(a, b) {
  return [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
}

function cross(a, b) {
  return [
    a[1] * b[2] - a[2] * b[1],
    a[2] * b[0] - a[0] * b[2],
    a[0] * b[1] - a[1] * b[0],
  ];
}

function dot(a, b) {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

function normalize(v) {
  const len = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  return [v[0] / len, v[1] / len, v[2] / len];
}

function generateGridXZ(size, spacing) {
  const positions = [];
  const offset = (size - 1) * spacing * 0.5;

  for (let i = 0; i < size; i++) {
    for (let j = 0; j < size; j++) {
      positions.push(j * spacing - offset, i * spacing - offset);
    }
  }
  return new Float32Array(positions);
}

function generateGridTriangleIndices(size) {
  const indices = [];

  for (let i = 0; i < size - 1; i++) {
    for (let j = 0; j < size - 1; j++) {
      const topLeft = i * size + j;
      const topRight = topLeft + 1;
      const bottomLeft = (i + 1) * size + j;
      const bottomRight = bottomLeft + 1;

      indices.push(topLeft, bottomLeft, topRight);
      indices.push(topRight, bottomLeft, bottomRight);
    }
  }

  return new Uint32Array(indices);
}

function generateSphere(subdivisions) {
  const positions = [];
  const normals = [];
  const indices = [];

  for (let lat = 0; lat <= subdivisions; lat++) {
    const theta = (lat * Math.PI) / subdivisions;
    const sinTheta = Math.sin(theta);
    const cosTheta = Math.cos(theta);

    for (let lon = 0; lon <= subdivisions; lon++) {
      const phi = (lon * 2 * Math.PI) / subdivisions;
      const sinPhi = Math.sin(phi);
      const cosPhi = Math.cos(phi);

      const x = cosPhi * sinTheta;
      const y = cosTheta;
      const z = sinPhi * sinTheta;

      positions.push(x, y, z);
      normals.push(x, y, z);
    }
  }

  for (let lat = 0; lat < subdivisions; lat++) {
    for (let lon = 0; lon < subdivisions; lon++) {
      const first = lat * (subdivisions + 1) + lon;
      const second = first + subdivisions + 1;

      indices.push(first, second, first + 1);
      indices.push(second, second + 1, first + 1);
    }
  }

  return {
    positions: new Float32Array(positions),
    normals: new Float32Array(normals),
    indices: new Uint32Array(indices),
  };
}

function createOrbitCamera(config) {
  return {
    radius: config.radius || 7.0,
    height: config.height || 4.0,
    speed: config.speed || 1.0,
    fov: config.fov || Math.PI / 4,
    near: config.near || 0.1,
    far: config.far || 100.0,
    target: config.target || [0, 0, 0],
    up: config.up || [0, 1, 0],
  };
}

function updateOrbitCamera(camera, time, aspect) {
  const angle = time * camera.speed;
  const eye = [
    Math.cos(angle) * camera.radius,
    camera.height,
    Math.sin(angle) * camera.radius,
  ];

  const projection = createPerspective(
    camera.fov,
    aspect,
    camera.near,
    camera.far,
  );
  const view = createLookAt(eye, camera.target, camera.up);
  return multiply(projection, view);
}

function createRenderPassConfig(config = {}) {
  return {
    clearColor: config.clearColor || { r: 0.1, g: 0.1, b: 0.1, a: 1.0 },
    loadOp: config.loadOp || "clear",
    storeOp: config.storeOp || "store",
    depthStencilConfig: config.depthStencilConfig || null,
  };
}

function beginColorRenderPass(encoder, context, config, depthTexture) {
  const passDescriptor = {
    colorAttachments: [
      {
        view: context.getCurrentTexture().createView(),
        clearValue: config.clearColor,
        loadOp: config.loadOp,
        storeOp: config.storeOp,
      },
    ],
    depthStencilAttachment: {
      view: depthTexture.createView(),
      depthClearValue: 1.0,
      depthLoadOp: "clear",
      depthStoreOp: "store",
    },
  };

  if (config.depthStencilConfig) {
    passDescriptor.depthStencilAttachment = {
      view: config.depthStencilConfig.view,
      depthClearValue: config.depthStencilConfig.clearValue || 1.0,
      depthLoadOp: config.depthStencilConfig.loadOp || "clear",
      depthStoreOp: config.depthStencilConfig.storeOp || "store",
    };
  }
  if (!encoder) {
    return null;
  }
  return encoder.beginRenderPass(passDescriptor);
}

function handleCanvasResize(canvas) {
  const width = window.innerWidth;
  const height = window.innerHeight;

  if (canvas.width !== width || canvas.height !== height) {
    canvas.width = width;
    canvas.height = height;
    return true;
  }
  return false;
}
function recreateTexture(device, canvas, depthTexture) {
  depthTexture.destroy();
  return device.createTexture({
    size: [canvas.width, canvas.height],
    format: "depth24plus",
    usage: GPUTextureUsage.RENDER_ATTACHMENT,
  });
}
function getAspectRatio(canvas) {
  return canvas.width / canvas.height;
}

function dispatchCompute(
  encoder,
  pipeline,
  bindGroup,
  workgroupX,
  workgroupY = workgroupX,
  workgroupZ = 1,
) {
  const pass = encoder.beginComputePass();
  pass.setPipeline(pipeline);
  pass.setBindGroup(0, bindGroup);
  pass.dispatchWorkgroups(workgroupX, workgroupY, workgroupZ);
  pass.end();
}

function drawIndexed(
  renderPass,
  pipeline,
  bindGroup,
  vertexBuffer,
  indexBuffer,
  indexCount,
) {
  renderPass.setPipeline(pipeline);
  renderPass.setBindGroup(0, bindGroup);
  if (Array.isArray(vertexBuffer)) {
    for (let i = 0; i < vertexBuffer.length; i++) {
      renderPass.setVertexBuffer(i, vertexBuffer[i]);
    }
  } else {
    renderPass.setVertexBuffer(0, vertexBuffer);
  }

  renderPass.setIndexBuffer(indexBuffer, "uint32");
  renderPass.drawIndexed(indexCount);
}

function createBlend() {
  return {
    color: {
      srcFactor: "src-alpha",
      dstFactor: "one-minus-src-alpha",
      operation: "add",
    },
    alpha: {
      srcFactor: "one",
      dstFactor: "one-minus-src-alpha",
      operation: "add",
    },
  };
}
function createDepthStencil() {
  return {
    depthWriteEnabled: true,
    depthCompare: "less",
    format: "depth24plus",
  };
}

function createDepthTexture(device) {
  return device.createTexture({
    size: [canvas.width, canvas.height],
    format: "depth24plus",
    usage: GPUTextureUsage.RENDER_ATTACHMENT,
  });
}

function dispatch(device, encoder, compute, buffers, workgroups) {
  const bindGroup = create_bind_group(
    device,
    compute.resources.bindGroupLayout,
    compute.info.bindings,
    buffers,
  );
  dispatchCompute(encoder, compute.pipeline, bindGroup, workgroups, workgroups);
}

function draw(
  device,
  pass,
  render,
  buffers,
  vertexBuffer,
  indexBuffer,
  indexCount,
) {
  const bindGroup = create_bind_group(
    device,
    render.resources.bindGroupLayout,
    render.vertex_info.bindings,
    buffers,
  );
  drawIndexed(
    pass,
    render.pipeline,
    bindGroup,
    vertexBuffer,
    indexBuffer,
    indexCount,
  );
}
function submit_commands(device, fn) {
  const encoder = device.createCommandEncoder();
  fn(encoder);
  device.queue.submit([encoder.finish()]);
}

function create_bind_group_layout(device, bindings) {
  const binding_map = new Map();

  for (const b of bindings) {
    const key = b.binding_index;

    const stage_flag =
      b.shader_stage === "vertex"
        ? GPUShaderStage.VERTEX
        : b.shader_stage === "fragment"
          ? GPUShaderStage.FRAGMENT
          : GPUShaderStage.COMPUTE;

    if (!binding_map.has(key)) {
      binding_map.set(key, {
        binding: b.binding_index,
        visibility: stage_flag,
        buffer_type: b.buffer_type,
      });
    } else {
      const entry = binding_map.get(key);
      entry.visibility |= stage_flag;
    }
  }

  const entries = Array.from(binding_map.values()).map((entry) => ({
    binding: entry.binding,
    visibility: entry.visibility,
    buffer: { type: entry.buffer_type },
  }));

  return device.createBindGroupLayout({ entries });
}

function create_pipeline_resources(device, shaders) {
  const all_bindings = shaders.flatMap((s) => s.bindings);
  const bindGroupLayout = create_bind_group_layout(device, all_bindings);
  const pipelineLayout = device.createPipelineLayout({
    bindGroupLayouts: [bindGroupLayout],
  });

  return { bindGroupLayout, pipelineLayout };
}

function create_compute_pipeline(device, shader_code) {
  const info = parse_shader(shader_code);
  const resources = create_pipeline_resources(device, [info]);

  const pipeline = device.createComputePipeline({
    layout: resources.pipelineLayout,
    compute: {
      module: device.createShaderModule({ code: shader_code }),
      entryPoint: "main",
    },
  });

  return { pipeline, resources, info };
}

function create_render_pipeline(
  device,
  vertex_code,
  fragment_code,
  format,
  topology = "line-list",
) {
  const vertex_info = parse_shader(vertex_code);
  const fragment_info = parse_shader(fragment_code);
  const resources = create_pipeline_resources(device, [
    vertex_info,
    fragment_info,
  ]);

  const vertex_buffers = vertex_info.vertex_attributes.map((attr) => ({
    arrayStride: wgsl_type_byte_size(parse_wgsl_type(attr.type_str).base_type),
    attributes: [
      {
        shaderLocation: attr.location,
        offset: 0,
        format: attr.format,
      },
    ],
  }));

  const pipeline = device.createRenderPipeline({
    layout: resources.pipelineLayout,
    vertex: {
      module: device.createShaderModule({ code: vertex_code }),
      entryPoint: "main",
      buffers: vertex_buffers,
    },
    fragment: {
      module: device.createShaderModule({ code: fragment_code }),
      entryPoint: "main",
      targets: [
        {
          format,
          blend: {
            color: {
              srcFactor: "src-alpha",
              dstFactor: "one-minus-src-alpha",
              operation: "add",
            },
            alpha: {
              srcFactor: "one",
              dstFactor: "one-minus-src-alpha",
              operation: "add",
            },
          },
        },
      ],
    },
    primitive: {
      topology,
      cullMode: "none",
    },
    depthStencil: {
      depthWriteEnabled: true,
      depthCompare: "less",
      format: "depth24plus",
    },
  });

  return { pipeline, resources, vertex_info, fragment_info };
}

function create_bind_group(device, layout, bindings, buffers) {
  const sorted = bindings.sort((a, b) => a.binding_index - b.binding_index);

  const entries = sorted.map((b, i) => ({
    binding: b.binding_index,
    resource: { buffer: buffers[i] },
  }));

  return device.createBindGroup({ layout, entries });
}