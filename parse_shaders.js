

function parse_shader_stage(code) {
  if (code.includes("@vertex")) return "vertex";
  if (code.includes("@fragment")) return "fragment";
  if (code.includes("@compute")) return "compute";
  return null;
}

function parse_workgroup_size(code) {
  const match = code.match(
    /@workgroup_size\((\d+)(?:,\s*(\d+))?(?:,\s*(\d+))?\)/,
  );
  if (match) {
    const x = parseInt(match[1]);
    const y = match[2] ? parseInt(match[2]) : 1;
    const z = match[3] ? parseInt(match[3]) : 1;
    return [x, y, z];
  }
  return null;
}

function parse_wgsl_type(type_str) {
  const trimmed = type_str.trim();
  const array_match = trimmed.match(/^array<(.+)>$/);
  if (array_match) {
    return { base_type: array_match[1].trim(), is_array: true };
  }
  return { base_type: trimmed, is_array: false };
}

function wgsl_type_to_vertex_format(type_str) {
  const map = {
    f32: "float32",
    "vec2<f32>": "float32x2",
    vec2f: "float32x2",
    "vec3<f32>": "float32x3",
    vec3f: "float32x3",
    "vec4<f32>": "float32x4",
    vec4f: "float32x4",
  };
  return map[type_str] || "float32x4";
}

function wgsl_type_byte_size(base_type) {
  const map = {
    f32: 4,
    u32: 4,
    i32: 4,
    vec2f: 8,
    "vec2<f32>": 8,
    vec3f: 12,
    "vec3<f32>": 12,
    vec4f: 16,
    "vec4<f32>": 16,
    "mat4x4<f32>": 64,
  };
  return map[base_type] || 16;
}

function infer_buffer_type(var_qualifier) {
  const trimmed = var_qualifier.trim();
  if (trimmed.startsWith("uniform")) return "uniform";
  if (trimmed.includes("read_write")) return "storage";
  if (trimmed.startsWith("storage")) {
    if (trimmed.includes(", read") || trimmed.includes(",read")) {
      return "read-only-storage";
    }
    return "read-only-storage";
  }
  return "uniform";
}

function parse_vertex_inputs(code) {
  const attrs = [];
  const struct_match = code.match(/struct\s+VertexInput\s*\{([^}]+)\}/s);
  if (!struct_match) return attrs;

  const struct_body = struct_match[1];
  const location_pattern = /@location\((\d+)\)\s+(\w+):\s*([^,\n]+)/g;
  let match;

  while ((match = location_pattern.exec(struct_body)) !== null) {
    const location = parseInt(match[1]);
    const name = match[2];
    const type_str = match[3].trim().replace(/,/g, "");
    const format = wgsl_type_to_vertex_format(type_str);

    attrs.push({ location, type_str, name, format });
  }

  return attrs.sort((a, b) => a.location - b.location);
}

function parse_bindings(code, stage) {
  const bindings = [];
  const pattern =
    /@binding\((\d+)\)\s+@group\((\d+)\)\s+var<([^>]+)>\s+(\w+)\s*:\s*([^;]+);/g;
  let match;

  while ((match = pattern.exec(code)) !== null) {
    const binding_index = parseInt(match[1]);
    const group_index = parseInt(match[2]);
    const var_qualifier = match[3];
    const variable_name = match[4];
    const var_type = match[5];

    const buffer_type = infer_buffer_type(var_qualifier);
    const wgsl_type = parse_wgsl_type(var_type);

    bindings.push({
      binding_index,
      group_index,
      buffer_type,
      variable_name,
      wgsl_type,
      shader_stage: stage,
    });
  }

  return bindings;
}

function parse_shader(code) {
  const stage = parse_shader_stage(code);
  if (!stage) return null;

  const bindings = parse_bindings(code, stage);
  const vertex_attributes = stage === "vertex" ? parse_vertex_inputs(code) : [];
  const workgroup_size =
    stage === "compute" ? parse_workgroup_size(code) : null;

  return {
    stage,
    bindings,
    vertex_attributes,
    workgroup_size,
  };
}
