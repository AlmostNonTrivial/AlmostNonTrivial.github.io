
// src/ir.ts
function instGetUses(inst) {
    switch (inst.op) {
        case "phi":
            return inst.sources.map((s) => s.vreg);
        case "mov":
            return [inst.src];
        case "loadi":
            return [];
        case "add":
        case "sub":
        case "mul":
        case "div":
        case "mod":
            return [inst.lhs, inst.rhs];
        case "cmp":
            return [inst.lhs, inst.rhs];
        case "alloca":
            return [];
        case "load":
            return [inst.ptr];
        case "store":
            return [inst.ptr, inst.value];
        case "call":
            return inst.args;
    }
}
function instGetDef(inst) {
    switch (inst.op) {
        case "phi":
        case "mov":
        case "loadi":
        case "add":
        case "sub":
        case "mul":
        case "div":
        case "mod":
        case "cmp":
        case "alloca":
        case "load":
            return inst.dst;
        case "call":
            return inst.dst;
        case "store":
    }
}
function updateInstDef(inst, newVReg) {
    switch (inst.op) {
        case "phi":
        case "mov":
        case "loadi":
        case "add":
        case "sub":
        case "mul":
        case "div":
        case "mod":
        case "cmp":
        case "load":
        case "alloca":
        case "call":
            if (inst.dst) {
                inst.dst = newVReg;
            }
            break;
        case "store":
            break;
    }
}
function termGetUses(term) {
    switch (term.op) {
        case "branch":
            return [term.cond];
        case "ret":
            return term.value ? [term.value] : [];
        case "jump":
            return [];
    }
}
function vregEqual(a, b) {
    return a.id === b.id;
}
function vregIsMemoryLocation(vreg) {
    return vreg.kind === "addr";
}
function vregToString(vreg) {
    switch (vreg.kind) {
        case "var":
            return `${vreg.sourceName}_${vreg.version}`;
        case "param":
            return `${vreg.sourceName}_${vreg.version}`;
        case "addr":
            return `&${vreg.sourceName}`;
        case "temp":
            return `t${vreg.id}`;
    }
}
function createIRGenerator(symbolTable) {
    return {
        nextId: 0,
        nextLabel: 1,
        varMap: /* @__PURE__ */ new Map(),
        labelMap: /* @__PURE__ */ new Map(),
        variables: /* @__PURE__ */ new Set(),
        addressedVars: /* @__PURE__ */ new Set(),
        definitions: /* @__PURE__ */ new Map(),
        currentBlockLabel: 0,
        instructions: [],
        symbolTable
    };
}
function createTemp(gen) {
    return { kind: "temp", id: gen.nextId++ };
}
function ensureLocation(gen, dst) {
    return dst ?? createTemp(gen);
}
function createVar(gen, sourceName, version = 0) {
    gen.variables.add(sourceName);
    return {
        kind: "var",
        id: gen.nextId++,
        sourceName,
        version
    };
}
function createParam(gen, sourceName, version = 0) {
    gen.variables.add(sourceName);
    return { kind: "param", id: gen.nextId++, sourceName, version };
}
function createAddr(gen, sourceName) {
    gen.addressedVars.add(sourceName);
    return { kind: "addr", id: gen.nextId++, sourceName };
}
function getLabel(gen, name) {
    if (!gen.labelMap.has(name)) {
        gen.labelMap.set(name, gen.nextLabel++);
    }
    return gen.labelMap.get(name);
}
function emit(gen, inst) {
    gen.instructions.push(inst);
}
function recordDefinition(gen, sourceName, label) {
    if (!gen.definitions.has(sourceName)) {
        gen.definitions.set(sourceName, /* @__PURE__ */ new Set());
    }
    gen.definitions.get(sourceName).add(label);
}
function getTypeSize(gen, type) {
    switch (type.kind) {
        case "int":
        case "pointer": {
            return 8;
        }
        case "array": {
            return type.size * getTypeSize(gen, type.base);
        }
        default: {
            return 8;
        }
    }
}
function genExpr(gen, expr, dst) {
    switch (expr.kind) {
        case "int_literal": {
            const target = ensureLocation(gen, dst);
            emit(gen, { op: "loadi", dst: target, imm: expr.value });
            return target;
        }
        case "identifier": {
            const sourceName = expr.name;
            const vreg = gen.varMap.get(sourceName);
            if (!vreg) {
                throw new Error(`Undefined variable: ${sourceName}`);
            }
            const sym = gen.symbolTable.get(sourceName);
            if (vreg.kind === "addr") {
                if (dst && !vregEqual(vreg, dst)) {
                    emit(gen, { op: "mov", dst, src: vreg });
                    return dst;
                }
                return vreg;
            }
            if (vregIsMemoryLocation(vreg)) {
                const target = ensureLocation(gen, dst);
                emit(gen, { op: "load", dst: target, ptr: vreg });
                return target;
            }
            if (dst && !vregEqual(vreg, dst)) {
                emit(gen, { op: "mov", dst, src: vreg });
                return dst;
            }
            return vreg;
        }
        case "binary": {
            const lhs = genExpr(gen, expr.left);
            const rhs = genExpr(gen, expr.right);
            const target = ensureLocation(gen, dst);
            switch (expr.op) {
                case "add":
                case "sub":
                case "mul":
                case "div":
                case "mod": {
                    emit(gen, { op: expr.op, dst: target, lhs, rhs });
                    break;
                }
                case "lt":
                case "gt":
                case "lte":
                case "gte":
                case "eq":
                case "neq": {
                    const condMap = {
                        lt: "lt",
                        gt: "gt",
                        lte: "le",
                        gte: "ge",
                        eq: "eq",
                        neq: "ne"
                    };
                    emit(gen, {
                        op: "cmp",
                        dst: target,
                        lhs,
                        rhs,
                        cond: condMap[expr.op]
                    });
                    break;
                }
                case "and":
                case "or": {
                    throw new Error(`Logical operators not yet implemented: ${expr.op}`);
                }
            }
            return target;
        }
        case "unary": {
            switch (expr.op) {
                case "not": {
                    const operand = genExpr(gen, expr.operand);
                    const zero = createTemp(gen);
                    emit(gen, { op: "loadi", dst: zero, imm: 0 });
                    const target = ensureLocation(gen, dst);
                    emit(gen, {
                        op: "cmp",
                        dst: target,
                        lhs: operand,
                        rhs: zero,
                        cond: "eq"
                    });
                    return target;
                }
                case "negate": {
                    const operand = genExpr(gen, expr.operand);
                    const zero = createTemp(gen);
                    emit(gen, { op: "loadi", dst: zero, imm: 0 });
                    const target = ensureLocation(gen, dst);
                    emit(gen, { op: "sub", dst: target, lhs: zero, rhs: operand });
                    return target;
                }
                case "address": {
                    const operand = expr.operand;
                    if (!(operand.kind === "identifier" || operand.kind === "index")) {
                        throw new Error("Can only take address of lvalue");
                    }
                    if (operand.kind === "identifier") {
                        const sourceName = operand.name;
                        const vreg = gen.varMap.get(sourceName);
                        if (!vreg) {
                            throw new Error(`Undefined variable: ${sourceName}`);
                        }
                        if (dst && !vregEqual(vreg, dst)) {
                            emit(gen, { op: "mov", dst, src: vreg });
                            return dst;
                        }
                        return vreg;
                    }
                    const arr = genExpr(gen, operand.array);
                    const index = genExpr(gen, operand.index);
                    const scale = createTemp(gen);
                    emit(gen, { op: "loadi", dst: scale, imm: 8 });
                    const offset = createTemp(gen);
                    emit(gen, { op: "mul", dst: offset, lhs: index, rhs: scale });
                    const resultDst = ensureLocation(gen, dst);
                    emit(gen, { op: "add", dst: resultDst, lhs: arr, rhs: offset });
                    return resultDst;
                }
                case "deref": {
                    const ptr = genExpr(gen, expr.operand);
                    const target = ensureLocation(gen, dst);
                    emit(gen, { op: "load", dst: target, ptr });
                    return target;
                }
                default: {
                    throw new Error(`Unsupported unary op: ${expr.op}`);
                }
            }
        }
        case "index": {
            const arr = genExpr(gen, expr.array);
            const index = genExpr(gen, expr.index);
            const scale = createTemp(gen);
            emit(gen, { op: "loadi", dst: scale, imm: 8 });
            const offset = createTemp(gen);
            emit(gen, { op: "mul", dst: offset, lhs: index, rhs: scale });
            const ptr = createTemp(gen);
            emit(gen, { op: "add", dst: ptr, lhs: arr, rhs: offset });
            const target = ensureLocation(gen, dst);
            emit(gen, { op: "load", dst: target, ptr });
            return target;
        }
        case "assign": {
            if (expr.target.kind === "unary" && expr.target.op === "deref") {
                const ptr = genExpr(gen, expr.target.operand);
                const rhs = genExpr(gen, expr.value);
                emit(gen, { op: "store", ptr, value: rhs });
                return rhs;
            }
            if (expr.target.kind === "index") {
                const arr = genExpr(gen, expr.target.array);
                const index = genExpr(gen, expr.target.index);
                const scale = createTemp(gen);
                emit(gen, { op: "loadi", dst: scale, imm: 8 });
                const offset = createTemp(gen);
                emit(gen, { op: "mul", dst: offset, lhs: index, rhs: scale });
                const ptr = createTemp(gen);
                emit(gen, { op: "add", dst: ptr, lhs: arr, rhs: offset });
                const rhs = genExpr(gen, expr.value);
                emit(gen, { op: "store", ptr, value: rhs });
                return rhs;
            }
            if (expr.target.kind !== "identifier") {
                throw new Error("Invalid assignment target");
            }
            const sourceName = expr.target.name;
            const vreg = gen.varMap.get(sourceName);
            if (!vreg) {
                throw new Error(`Undefined variable: ${sourceName}`);
            }
            if (vregIsMemoryLocation(vreg)) {
                const rhs = genExpr(gen, expr.value);
                emit(gen, { op: "store", ptr: vreg, value: rhs });
                return rhs;
            } else {
                recordDefinition(gen, sourceName, gen.currentBlockLabel);
                genExpr(gen, expr.value, vreg);
                return vreg;
            }
        }
        case "call": {
            const funcName = expr.func.kind === "identifier" ? expr.func.name : "";
            if (!funcName) {
                throw new Error("Function calls must be direct");
            }
            const args = expr.args.map((arg) => genExpr(gen, arg));
            const funcSym = gen.symbolTable.get(funcName);
            const hasReturn = funcSym?.declaration?.kind === "func" && funcSym.declaration.type.kind === "function" && funcSym.declaration.type.returnType.kind !== "void";
            if (hasReturn) {
                const target = ensureLocation(gen, dst);
                emit(gen, { op: "call", dst: target, func: funcName, args });
                return target;
            } else {
                emit(gen, { op: "call", func: funcName, args });
                return createTemp(gen);
            }
        }
        default: {
            throw new Error(`Unsupported expr: ${expr.kind}`);
        }
    }
}
function genStmt(gen, stmt) {
    if (stmt.label && stmt.label !== "") {
        const label = getLabel(gen, stmt.label);
        gen.currentBlockLabel = label;
        emit(gen, { label: stmt.label });
    }
    switch (stmt.kind) {
        case "expr": {
            genExpr(gen, stmt.expr);
            break;
        }
        case "return": {
            if (stmt.value) {
                const value = genExpr(gen, stmt.value);
                emit(gen, { op: "ret", value });
            } else {
                emit(gen, { op: "ret" });
            }
            break;
        }
        case "goto": {
            const last = gen.instructions[gen.instructions.length - 1];
            if ("op" in last && last.op === "branch") {
                const branch = last;
                if (branch.falseTarget === -1) {
                    branch.falseTarget = getLabel(gen, stmt.target);
                    break;
                }
            }
            emit(gen, { op: "jump", target: getLabel(gen, stmt.target) });
            break;
        }
        case "if": {
            const cond = genExpr(gen, stmt.cond);
            if (stmt.thenStmt.kind !== "goto") {
                throw new Error("Normalized IF must have GOTO in then branch");
            }
            if (!stmt.elseStmt || stmt.elseStmt.kind !== "goto") {
                throw new Error("Normalized IF must have GOTO in else branch");
            }
            const trueTarget = getLabel(gen, stmt.thenStmt.target);
            const falseTarget = getLabel(gen, stmt.elseStmt.target);
            emit(gen, { op: "branch", cond, trueTarget, falseTarget });
            break;
        }
        case "block": {
            for (const s of stmt.stmts) {
                genStmt(gen, s);
            }
            break;
        }
        case "decl": {
            const decl = stmt.decl;
            if (decl.kind !== "var") {
                break;
            }
            const sourceName = decl.name;
            const sym = gen.symbolTable.get(sourceName);
            if (decl.type.kind === "array") {
                const addrVreg = createAddr(gen, sourceName);
                const size = getTypeSize(gen, decl.type);
                emit(gen, { op: "alloca", dst: addrVreg, size });
                gen.varMap.set(sourceName, addrVreg);
            } else if (sym?.isAddressed) {
                const addrVreg = createAddr(gen, sourceName);
                const size = getTypeSize(gen, decl.type);
                emit(gen, { op: "alloca", dst: addrVreg, size });
                gen.varMap.set(sourceName, addrVreg);
                if (decl.init) {
                    const value = genExpr(gen, decl.init);
                    emit(gen, { op: "store", ptr: addrVreg, value });
                }
            } else {
                const varVreg = createVar(gen, sourceName, 0);
                gen.varMap.set(sourceName, varVreg);
                recordDefinition(gen, sourceName, gen.currentBlockLabel);
                if (decl.init) {
                    genExpr(gen, decl.init, varVreg);
                }
            }
            break;
        }
        case "empty": {
            break;
        }
        case "while":
        case "for": {
            throw new Error(`${stmt.kind} should be lowered before IR generation`);
        }
        default: {
            throw new Error(`Unexpected stmt: ${stmt.kind}`);
        }
    }
}
function connectBlocks(gen, name, params) {
    const blocks = /* @__PURE__ */ new Map();
    const TERMINATORS = ["ret", "jump", "branch"];
    const REGULAR_INSTRUCTIONS = [
        "mov",
        "loadi",
        "add",
        "sub",
        "mul",
        "div",
        "mod",
        "cmp",
        "alloca",
        "load",
        "store",
        "call",
        "phi"
    ];
    let currentBlock = {
        label: 0,
        instructions: [],
        terminator: null
    };
    blocks.set(0, currentBlock);
    for (const item of gen.instructions) {
        if ("label" in item) {
            const label = getLabel(gen, item.label);
            if (currentBlock && !currentBlock.terminator) {
                currentBlock.terminator = { op: "jump", target: label };
            }
            currentBlock = { label, instructions: [], terminator: null };
            blocks.set(label, currentBlock);
        } else if ("op" in item && TERMINATORS.includes(item.op)) {
            if (currentBlock) {
                currentBlock.terminator = item;
                currentBlock = null;
            }
        } else if ("op" in item && REGULAR_INSTRUCTIONS.includes(item.op)) {
            if (currentBlock && !currentBlock.terminator) {
                currentBlock.instructions.push(item);
            }
        } else {
            throw new Error(`Unknown instruction type: ${JSON.stringify(item)}`);
        }
    }
    if (currentBlock && !currentBlock.terminator) {
        currentBlock.terminator = { op: "ret" };
    }
    return {
        name,
        params,
        blocks,
        entryBlock: 0,
        ssaInfo: {
            variables: new Set(gen.variables),
            addressed: new Set(gen.addressedVars),
            definitions: new Map(gen.definitions)
        }
    };
}
function genFunction(gen, decl) {
    if (decl.kind !== "func") {
        throw new Error("Expected function declaration");
    }
    gen.nextId = 0;
    gen.nextLabel = 1;
    gen.instructions = [];
    gen.varMap.clear();
    gen.labelMap.clear();
    gen.variables.clear();
    gen.addressedVars.clear();
    gen.definitions.clear();
    gen.currentBlockLabel = 0;
    const params = [];
    for (const param of decl.params) {
        const paramVreg = createParam(gen, param.name, 0);
        params.push(paramVreg);
        const sym = gen.symbolTable.get(param.name);
        if (sym?.isAddressed) {
            const addrVreg = createAddr(gen, param.name);
            const size = getTypeSize(gen, param.type);
            emit(gen, { op: "alloca", dst: addrVreg, size });
            emit(gen, { op: "store", ptr: addrVreg, value: paramVreg });
            gen.varMap.set(param.name, addrVreg);
        } else {
            gen.varMap.set(param.name, paramVreg);
            recordDefinition(gen, param.name, 0);
        }
    }
    if (decl.body) {
        genStmt(gen, decl.body);
    }
    return connectBlocks(gen, decl.name, params);
}
function genProgram(gen, program) {
    const functions = [];
    const callGraph = /* @__PURE__ */ new Map();
    for (const decl of program.declarations) {
        if (decl.kind === "func" && decl.body) {
            functions.push(genFunction(gen, decl));
        }
    }
    const functionCFGs = /* @__PURE__ */ new Map();
    for (const func of functions) {
        const preds = /* @__PURE__ */ new Map();
        const succs = /* @__PURE__ */ new Map();
        const exits = /* @__PURE__ */ new Set();
        const calls = /* @__PURE__ */ new Set();
        for (const [label, block] of func.blocks) {
            switch (block.terminator.op) {
                case "jump": {
                    const target = block.terminator.target;
                    if (!succs.has(label)) {
                        succs.set(label, /* @__PURE__ */ new Set());
                    }
                    succs.get(label).add(target);
                    if (!preds.has(target)) {
                        preds.set(target, /* @__PURE__ */ new Set());
                    }
                    preds.get(target).add(label);
                    break;
                }
                case "branch": {
                    const { trueTarget, falseTarget } = block.terminator;
                    if (!succs.has(label)) {
                        succs.set(label, /* @__PURE__ */ new Set());
                    }
                    succs.get(label).add(trueTarget);
                    succs.get(label).add(falseTarget);
                    if (!preds.has(trueTarget)) {
                        preds.set(trueTarget, /* @__PURE__ */ new Set());
                    }
                    preds.get(trueTarget).add(label);
                    if (!preds.has(falseTarget)) {
                        preds.set(falseTarget, /* @__PURE__ */ new Set());
                    }
                    preds.get(falseTarget).add(label);
                    break;
                }
                case "ret": {
                    exits.add(label);
                    break;
                }
            }
            for (const inst of block.instructions) {
                if (inst.op === "call") {
                    calls.add(inst.func);
                }
            }
        }
        functionCFGs.set(func.name, {
            entry: func.entryBlock,
            exit: exits,
            predecessors: preds,
            successors: succs
        });
        callGraph.set(func.name, calls);
    }
    return { functions, cfg: { callGraph, functionCFGs } };
}
function generateIR(program, symbolTable) {
    const gen = createIRGenerator(symbolTable);
    return genProgram(gen, program);
}

// src/shared.ts
function setsEqual(a, b) {
    if (a.size !== b.size) return false;
    for (const entry of a) {
        if (!b.has(entry)) {
            return false;
        }
    }
    return true;
}
function assert(pred, msg) {
    if (!pred) {
        throw new Error(msg);
    }
}

// src/asm.ts
function emitProgram(program, allocations) {
    const output = [];
    output.push(".section __TEXT,__text");
    output.push(".globl _main");
    output.push(".p2align 2");
    output.push("");
    for (const func of program.functions) {
        const allocation = allocations.get(func.name);
        const funcAsm = emitFunction(func, allocation);
        output.push(...funcAsm);
        output.push("");
    }
    return peepholeOptimize(output).join("\n");
}
function emitFunction(func, allocation) {
    const ctx = {
        func,
        allocation,
        frame: calculateFrameLayout(func),
        output: []
    };
    ctx.output.push("_main:");
    emitPrologue(ctx);
    for (const [blockLabel, block] of func.blocks) {
        if (blockLabel !== func.entryBlock) {
            ctx.output.push(`.L${blockLabel}:`);
        }
        for (const inst of block.instructions) {
            emitInstruction(ctx, inst);
        }
        emitTerminator(ctx, block.terminator);
    }
    return ctx.output;
}
function calculateFrameLayout(func) {
    const layout = {
        allocaOffsets: /* @__PURE__ */ new Map(),
        totalAllocaSize: 0,
        frameSize: 0
    };
    let currentOffset = 0;
    for (const block of func.blocks.values()) {
        for (const inst of block.instructions) {
            if (inst.op === "alloca") {
                layout.allocaOffsets.set(inst.dst.id, currentOffset);
                currentOffset += inst.size;
            }
        }
    }
    layout.totalAllocaSize = currentOffset;
    layout.frameSize = currentOffset + 15 & ~15;
    return layout;
}
function emitPrologue(ctx) {
    if (ctx.frame.frameSize === 0) {
        return;
    }
    ctx.output.push("    stp x29, x30, [sp, #-16]!");
    ctx.output.push("    mov x29, sp");
    if (ctx.frame.frameSize > 0) {
        if (ctx.frame.frameSize <= 4095) {
            ctx.output.push(`    sub sp, sp, #${ctx.frame.frameSize}`);
        } else {
            ctx.output.push(`    mov x16, #${ctx.frame.frameSize}`);
            ctx.output.push("    sub sp, sp, x16");
        }
    }
}
function emitEpilogue(ctx) {
    if (ctx.frame.frameSize === 0) {
        ctx.output.push("    ret");
        return;
    }
    if (ctx.frame.frameSize > 0) {
        if (ctx.frame.frameSize <= 4095) {
            ctx.output.push(`    add sp, sp, #${ctx.frame.frameSize}`);
        } else {
            ctx.output.push(`    mov x16, #${ctx.frame.frameSize}`);
            ctx.output.push("    add sp, sp, x16");
        }
    }
    ctx.output.push("    ldp x29, x30, [sp], #16");
    ctx.output.push("    ret");
}
function getReg(ctx, vreg) {
    assert(
        vreg.kind != "addr",
        `Address VReg ${vreg.id} (${vregToString(vreg)}) shouldn't be accessed as a register`
    );
    const regNum = ctx.allocation.assignment.get(vreg.id);
    assert(
        regNum != void 0,
        `No register assigned to VReg ${vreg.id} (${vregToString(vreg)})`
    );
    return `x${regNum}`;
}
function getOperand(ctx, vreg, tempReg = "x17") {
    if (vreg.kind === "addr") {
        const offset = ctx.frame.allocaOffsets.get(vreg.id);
        if (offset === 0) {
            ctx.output.push(`    mov ${tempReg}, sp`);
        } else {
            ctx.output.push(`    add ${tempReg}, sp, #${offset}`);
        }
        return tempReg;
    }
    return getReg(ctx, vreg);
}
function emitInstruction(ctx, inst) {
    switch (inst.op) {
        case "phi":
            break;
        case "mov": {
            if (inst.src.kind === "addr") {
                const dst = getReg(ctx, inst.dst);
                const offset = ctx.frame.allocaOffsets.get(inst.src.id);
                assert(
                    offset != void 0,
                    `No offset for address VReg ${inst.src.id} (${vregToString(inst.src)})`
                );
                if (offset === 0) {
                    ctx.output.push(`    mov ${dst}, sp`);
                } else {
                    ctx.output.push(`    add ${dst}, sp, #${offset}`);
                }
            } else {
                const dst = getReg(ctx, inst.dst);
                const src = getReg(ctx, inst.src);
                if (dst !== src) {
                    ctx.output.push(`    mov ${dst}, ${src}`);
                }
            }
            break;
        }
        case "loadi": {
            const dst = getReg(ctx, inst.dst);
            if (inst.imm >= 0 && inst.imm < 65536) {
                ctx.output.push(`    mov ${dst}, #${inst.imm}`);
            } else if (inst.imm < 0 && inst.imm >= -65536) {
                ctx.output.push(`    mov ${dst}, #${inst.imm}`);
            } else {
                const low = inst.imm & 65535;
                const high = inst.imm >> 16 & 65535;
                ctx.output.push(`    movz ${dst}, #${low}`);
                if (high !== 0) {
                    ctx.output.push(`    movk ${dst}, #${high}, lsl #16`);
                }
            }
            break;
        }
        case "add": {
            const dst = getReg(ctx, inst.dst);
            const lhs = getOperand(ctx, inst.lhs, "x16");
            const rhs = getOperand(ctx, inst.rhs, "x17");
            ctx.output.push(`    add ${dst}, ${lhs}, ${rhs}`);
            break;
        }
        case "sub": {
            const dst = getReg(ctx, inst.dst);
            const lhs = getOperand(ctx, inst.lhs, "x16");
            const rhs = getOperand(ctx, inst.rhs, "x17");
            ctx.output.push(`    sub ${dst}, ${lhs}, ${rhs}`);
            break;
        }
        case "mul": {
            const dst = getReg(ctx, inst.dst);
            const lhs = getOperand(ctx, inst.lhs, "x16");
            const rhs = getOperand(ctx, inst.rhs, "x17");
            ctx.output.push(`    mul ${dst}, ${lhs}, ${rhs}`);
            break;
        }
        case "div": {
            const dst = getReg(ctx, inst.dst);
            const lhs = getOperand(ctx, inst.lhs, "x16");
            const rhs = getOperand(ctx, inst.rhs, "x17");
            ctx.output.push(`    sdiv ${dst}, ${lhs}, ${rhs}`);
            break;
        }
        case "mod": {
            const dst = getReg(ctx, inst.dst);
            const lhs = getOperand(ctx, inst.lhs, "x16");
            const rhs = getOperand(ctx, inst.rhs, "x17");
            ctx.output.push(`    sdiv x18, ${lhs}, ${rhs}`);
            ctx.output.push(`    msub ${dst}, x18, ${rhs}, ${lhs}`);
            break;
        }
        case "cmp": {
            const dst = getReg(ctx, inst.dst);
            const lhs = getOperand(ctx, inst.lhs, "x16");
            const rhs = getOperand(ctx, inst.rhs, "x17");
            ctx.output.push(`    cmp ${lhs}, ${rhs}`);
            const condMap = {
                eq: "eq",
                ne: "ne",
                lt: "lt",
                le: "le",
                gt: "gt",
                ge: "ge"
            };
            const cond = condMap[inst.cond];
            ctx.output.push(`    cset ${dst}, ${cond}`);
            break;
        }
        case "alloca": {
            break;
        }
        case "load": {
            const dst = getReg(ctx, inst.dst);
            const ptr = getOperand(ctx, inst.ptr, "x16");
            ctx.output.push(`    ldr ${dst}, [${ptr}]`);
            break;
        }
        case "store": {
            const ptr = getOperand(ctx, inst.ptr, "x16");
            const value = getOperand(ctx, inst.value, "x17");
            ctx.output.push(`    str ${value}, [${ptr}]`);
            break;
        }
        case "call": {
            assert(false, "Function calls not supported in single-function mode");
            break;
        }
    }
}
function emitTerminator(ctx, term) {
    switch (term.op) {
        case "jump": {
            ctx.output.push(`    b .L${term.target}`);
            break;
        }
        case "branch": {
            const cond = getOperand(ctx, term.cond, "x16");
            ctx.output.push(`    cmp ${cond}, #0`);
            ctx.output.push(`    b.ne .L${term.trueTarget}`);
            ctx.output.push(`    b .L${term.falseTarget}`);
            break;
        }
        case "ret": {
            if (term.value) {
                const value = getOperand(ctx, term.value, "x16");
                if (value !== "x0") {
                    ctx.output.push(`    mov x0, ${value}`);
                }
            }
            emitEpilogue(ctx);
            break;
        }
    }
}
function peepholeOptimize(lines) {
    const result = [];
    for (let i = 0; i < lines.length; i++) {
        const curr = lines[i].trim();
        const next = lines[i + 1]?.trim();
        const movImmMatch = curr.match(/^\s*mov (x\d+), #(\d+)$/);
        if (movImmMatch && next) {
            const [, reg, imm] = movImmMatch;
            const movX0Match = next.match(/^\s*mov x0, (x\d+)$/);
            if (movX0Match && movX0Match[1] === reg) {
                result.push(`    mov x0, #${imm}`);
                i++;
                continue;
            }
        }
        result.push(lines[i]);
    }
    return result;
}

// src/types.ts
function typesEqual(a, b) {
    if (a.kind !== b.kind) return false;
    switch (a.kind) {
        case "void":
        case "int":
            return true;
        case "pointer":
            return b.kind === "pointer" && typesEqual(a.base, b.base);
        case "array":
            return b.kind === "array" && a.size === b.size && typesEqual(a.base, b.base);
        case "function":
            if (b.kind !== "function") return false;
            if (!typesEqual(a.returnType, b.returnType)) return false;
            if (a.paramTypes.length !== b.paramTypes.length) return false;
            for (let i = 0; i < a.paramTypes.length; i++) {
                if (!typesEqual(a.paramTypes[i], b.paramTypes[i])) return false;
            }
            return true;
    }
}
function isIntegerType(type) {
    return type.kind === "int";
}
function isPointerType(type) {
    return type.kind === "pointer";
}
function isArithmeticType(type) {
    return type.kind === "int";
}
function decayType(type) {
    if (type.kind === "array") {
        return { kind: "pointer", base: type.base };
    }
    return type;
}
function symbolName(functionName, variableName) {
    return `${functionName}.${variableName}`;
}
function makeSymbolName(state, name) {
    if (state.currentFunction) {
        return symbolName(state.currentFunction, name);
    }
    return name;
}
function lookupSymbol(state, name) {
    if (state.currentFunction) {
        const localName = `${state.currentFunction}.${name}`;
        const local = state.symbolTable.get(localName);
        if (local) return local;
    }
    return state.symbolTable.get(name) || null;
}
function declareSymbol(state, name, decl) {
    const fullName = makeSymbolName(state, name);
    if (state.symbolTable.has(fullName)) {
        state.error = `Redefinition of '${name}'`;
        return false;
    }
    const isDefinitelyAddressed = decl.kind === "var" && decl.type.kind === "array";
    const symbol = {
        name: fullName,
        declaration: decl,
        isAddressed: isDefinitelyAddressed
    };
    state.symbolTable.set(fullName, symbol);
    return true;
}
function checkExpr(state, expr) {
    if (state.error) return null;
    switch (expr.kind) {
        case "int_literal":
            return { kind: "int" };
        case "identifier": {
            const symbol = lookupSymbol(state, expr.name);
            if (!symbol) {
                state.error = `Undefined identifier '${expr.name}'`;
                return null;
            }
            const decl = symbol.declaration;
            if (decl.kind === "var") {
                return decayType(decl.type);
            } else if (decl.kind === "func") {
                return decl.type;
            }
            return null;
        }
        case "binary": {
            const leftType = checkExpr(state, expr.left);
            if (!leftType) return null;
            const rightType = checkExpr(state, expr.right);
            if (!rightType) return null;
            switch (expr.op) {
                case "add":
                case "sub": {
                    if (isIntegerType(leftType) && isIntegerType(rightType)) {
                        return { kind: "int" };
                    }
                    if (isPointerType(leftType) && isIntegerType(rightType)) {
                        return leftType;
                    }
                    if (expr.op === "add" && isIntegerType(leftType) && isPointerType(rightType)) {
                        return rightType;
                    }
                    if (expr.op === "sub" && isPointerType(leftType) && isPointerType(rightType)) {
                        if (!typesEqual(leftType, rightType)) {
                            state.error = "Pointer subtraction requires same pointer types";
                            return null;
                        }
                        return { kind: "int" };
                    }
                    state.error = `Invalid operands to '${expr.op}'`;
                    return null;
                }
                case "mul":
                case "div":
                case "mod":
                    if (!isArithmeticType(leftType) || !isArithmeticType(rightType)) {
                        state.error = `Invalid operands to '${expr.op}' (requires arithmetic types)`;
                        return null;
                    }
                    return { kind: "int" };
                case "lt":
                case "gt":
                case "lte":
                case "gte":
                case "eq":
                case "neq":
                    if (isIntegerType(leftType) && isIntegerType(rightType)) {
                        return { kind: "int" };
                    }
                    if (isPointerType(leftType) && isPointerType(rightType)) {
                        if (!typesEqual(leftType, rightType)) {
                            state.error = "Comparison requires same pointer types";
                            return null;
                        }
                        return { kind: "int" };
                    }
                    state.error = `Invalid operands to '${expr.op}'`;
                    return null;
                case "and":
                case "or":
                    if (!isIntegerType(leftType) || !isIntegerType(rightType)) {
                        state.error = `Logical operators require integer operands`;
                        return null;
                    }
                    return { kind: "int" };
            }
        }
        case "unary": {
            const operandType = checkExpr(state, expr.operand);
            if (!operandType) return null;
            switch (expr.op) {
                case "negate":
                case "not":
                    if (!isIntegerType(operandType)) {
                        state.error = `Operator '${expr.op}' requires integer operand`;
                        return null;
                    }
                    return { kind: "int" };
                case "address":
                    if (expr.operand.kind === "identifier") {
                        const sym = state.symbolTable.get(expr.operand.name);
                        sym.isAddressed = true;
                    } else if (expr.operand.kind !== "unary" && expr.operand.kind !== "index") {
                        state.error = "Cannot take address of rvalue";
                        return null;
                    }
                    return { kind: "pointer", base: operandType };
                case "deref":
                    if (!isPointerType(operandType)) {
                        state.error = "Dereference requires pointer operand";
                        return null;
                    }
                    return operandType.base;
                case "pre_inc":
                case "pre_dec":
                case "post_inc":
                case "post_dec":
                    if (!isIntegerType(operandType) && !isPointerType(operandType)) {
                        state.error = `Increment/decrement requires integer or pointer`;
                        return null;
                    }
                    return operandType;
            }
        }
        case "assign": {
            const targetType = checkExpr(state, expr.target);
            if (!targetType) return null;
            if (expr.target.kind !== "identifier" && expr.target.kind !== "unary" && expr.target.kind !== "index") {
                state.error = "Assignment requires lvalue";
                return null;
            }
            const valueType = checkExpr(state, expr.value);
            if (!valueType) return null;
            if (!typesEqual(targetType, valueType)) {
                state.error = "Type mismatch in assignment";
                return null;
            }
            return targetType;
        }
        case "call": {
            const funcType = checkExpr(state, expr.func);
            if (!funcType) return null;
            if (funcType.kind !== "function") {
                state.error = "Call requires function";
                return null;
            }
            if (expr.args.length !== funcType.paramTypes.length) {
                state.error = `Function expects ${funcType.paramTypes.length} arguments, got ${expr.args.length}`;
                return null;
            }
            for (let i = 0; i < expr.args.length; i++) {
                const argType = checkExpr(state, expr.args[i]);
                if (!argType) return null;
                if (!typesEqual(argType, funcType.paramTypes[i])) {
                    state.error = `Argument ${i + 1} type mismatch`;
                    return null;
                }
            }
            return funcType.returnType;
        }
        case "index": {
            const arrayType = checkExpr(state, expr.array);
            if (!arrayType) return null;
            const indexType = checkExpr(state, expr.index);
            if (!indexType) return null;
            if (!isIntegerType(indexType)) {
                state.error = "Array index must be integer";
                return null;
            }
            if (arrayType.kind === "pointer") {
                return arrayType.base;
            } else if (arrayType.kind === "array") {
                return arrayType.base;
            } else {
                state.error = "Indexing requires array or pointer";
                return null;
            }
        }
    }
}
function checkStmt(state, stmt, returnType) {
    if (state.error) return false;
    switch (stmt.kind) {
        case "expr":
            checkExpr(state, stmt.expr);
            return state.error === null;
        case "return":
            if (stmt.value) {
                const valueType = checkExpr(state, stmt.value);
                if (!valueType) return false;
                if (returnType.kind === "void") {
                    state.error = "Return with value in void function";
                    return false;
                }
                if (!typesEqual(valueType, returnType)) {
                    state.error = "Return type mismatch";
                    return false;
                }
            } else {
                if (returnType.kind !== "void") {
                    state.error = "Return without value in non-void function";
                    return false;
                }
            }
            return true;
        case "if":
            const condType = checkExpr(state, stmt.cond);
            if (!condType) return false;
            if (!isIntegerType(condType)) {
                state.error = "Condition must be integer";
                return false;
            }
            if (!checkStmt(state, stmt.thenStmt, returnType)) return false;
            if (stmt.elseStmt && !checkStmt(state, stmt.elseStmt, returnType))
                return false;
            return true;
        case "while":
            const whileCondType = checkExpr(state, stmt.cond);
            if (!whileCondType) return false;
            if (!isIntegerType(whileCondType)) {
                state.error = "Condition must be integer";
                return false;
            }
            return checkStmt(state, stmt.body, returnType);
        case "for":
            if (stmt.init && !checkStmt(state, stmt.init, returnType)) return false;
            if (stmt.cond) {
                const forCondType = checkExpr(state, stmt.cond);
                if (!forCondType) return false;
                if (!isIntegerType(forCondType)) {
                    state.error = "Condition must be integer";
                    return false;
                }
            }
            if (stmt.update) {
                checkExpr(state, stmt.update);
                if (state.error) return false;
            }
            return checkStmt(state, stmt.body, returnType);
        case "block":
            for (const s of stmt.stmts) {
                if (!checkStmt(state, s, returnType)) return false;
            }
            return true;
        case "decl":
            if (stmt.decl.kind === "var") {
                if (!declareSymbol(state, stmt.decl.name, stmt.decl)) return false;
                if (stmt.decl.init) {
                    const initType = checkExpr(state, stmt.decl.init);
                    if (!initType) return false;
                    const varType = decayType(stmt.decl.type);
                    if (!typesEqual(varType, initType)) {
                        state.error = "Initializer type mismatch";
                        return false;
                    }
                }
            }
            return true;
        case "goto":
        case "empty":
            return true;
    }
}
function checkFunction(state, decl) {
    if (decl.kind !== "func") return true;
    state.currentFunction = decl.name;
    for (const param of decl.params) {
        const paramDecl = {
            kind: "var",
            name: param.name,
            type: param.type,
            init: null
        };
        if (!declareSymbol(state, param.name, paramDecl)) {
            return false;
        }
    }
    if (decl.body) {
        const returnType = decl.type.kind === "function" ? decl.type.returnType : { kind: "void" };
        if (!checkStmt(state, decl.body, returnType)) {
            return false;
        }
    }
    state.currentFunction = null;
    return true;
}
function semantic_analyze(program) {
    const state = {
        symbolTable: /* @__PURE__ */ new Map(),
        currentFunction: null,
        error: null
    };
    for (const decl of program.declarations) {
        if (decl.kind === "func") {
            if (!declareSymbol(state, decl.name, decl)) {
                return {
                    symbolTable: state.symbolTable,
                    error: state.error || "Failed to declare function"
                };
            }
        }
    }
    for (const decl of program.declarations) {
        if (decl.kind === "var") {
            if (!declareSymbol(state, decl.name, decl)) {
                return {
                    symbolTable: state.symbolTable,
                    error: state.error || "Failed to declare global"
                };
            }
            if (decl.init) {
                const initType = checkExpr(state, decl.init);
                if (!initType || state.error) {
                    return {
                        symbolTable: state.symbolTable,
                        error: state.error || "Invalid initializer"
                    };
                }
                if (!typesEqual(decayType(decl.type), initType)) {
                    return {
                        symbolTable: state.symbolTable,
                        error: "Global initializer type mismatch"
                    };
                }
            }
        } else if (decl.kind === "func") {
            if (!checkFunction(state, decl)) {
                return {
                    symbolTable: state.symbolTable,
                    error: state.error || "Function check failed"
                };
            }
        }
    }
    return {
        symbolTable: state.symbolTable,
        error: ""
    };
}

// src/ast_normalizer.ts
function createExprLowerer(symbolTable) {
    return {
        tempCounter: 100,
        pendingStmts: [],
        symbolTable
    };
}
function createCFNormalizer() {
    return {
        labelCounter: 1,
        tempCounter: 0
    };
}
function genLabel(state, prefix = "L") {
    return `_${prefix}${state.labelCounter++}`;
}
function inferExprType(expr, symbolTable) {
    switch (expr.kind) {
        case "int_literal":
            return { kind: "int" };
        case "identifier": {
            const sym = symbolTable.get(expr.name);
            if (!sym) return { kind: "int" };
            const decl = sym.declaration;
            if (decl.kind === "var") {
                return decl.type;
            }
            return { kind: "int" };
        }
        case "binary":
            const leftType = inferExprType(expr.left, symbolTable);
            if (expr.op === "add" || expr.op === "sub") {
                if (leftType.kind === "pointer") return leftType;
                const rightType = inferExprType(expr.right, symbolTable);
                if (rightType.kind === "pointer") return rightType;
            }
            return { kind: "int" };
        case "unary":
            if (expr.op === "address") {
                const operandType = inferExprType(expr.operand, symbolTable);
                return { kind: "pointer", base: operandType };
            }
            if (expr.op === "deref") {
                const operandType = inferExprType(expr.operand, symbolTable);
                if (operandType.kind === "pointer") {
                    return operandType.base;
                }
            }
            return inferExprType(expr.operand, symbolTable);
        case "assign":
            return inferExprType(expr.target, symbolTable);
        case "call":
            return { kind: "int" };
        case "index":
            const arrayType = inferExprType(expr.array, symbolTable);
            if (arrayType.kind === "pointer") {
                return arrayType.base;
            }
            if (arrayType.kind === "array") {
                return arrayType.base;
            }
            return { kind: "int" };
    }
}
function lowerArrayAddress(state, array, index) {
    let loweredArray = lowerExpr(state, array);
    let loweredIndex = lowerExpr(state, index);
    const scale = { kind: "int_literal", value: 8 };
    const scaledIndex = makeTempVar(state, { kind: "int" });
    state.pendingStmts.push({
        kind: "expr",
        label: "",
        expr: {
            kind: "assign",
            target: scaledIndex,
            value: { kind: "binary", op: "mul", left: loweredIndex, right: scale }
        }
    });
    const arrayType = inferExprType(loweredArray, state.symbolTable);
    const baseType = arrayType.kind === "pointer" ? arrayType.base : arrayType.kind === "array" ? arrayType.base : { kind: "int" };
    const ptr = makeTempVar(state, { kind: "pointer", base: baseType });
    state.pendingStmts.push({
        kind: "expr",
        label: "",
        expr: {
            kind: "assign",
            target: ptr,
            value: {
                kind: "binary",
                op: "add",
                left: loweredArray,
                right: scaledIndex
            }
        }
    });
    return ptr;
}
function lowerExpr(state, expr) {
    switch (expr.kind) {
        case "int_literal":
        case "identifier":
            return expr;
        case "binary": {
            if (expr.op === "and" || expr.op === "or") {
                let left2 = lowerExpr(state, expr.left);
                if (!isSimple(left2)) {
                    const temp = makeTempVar(state, { kind: "int" });
                    state.pendingStmts.push({
                        kind: "expr",
                        label: "",
                        expr: { kind: "assign", target: temp, value: left2 }
                    });
                    left2 = temp;
                }
                const result = makeTempVar(state, { kind: "int" });
                const skipLabel = genTempLowerer(state);
                const endLabel = genTempLowerer(state);
                if (expr.op === "and") {
                    state.pendingStmts.push({
                        kind: "expr",
                        label: "",
                        expr: {
                            kind: "assign",
                            target: result,
                            value: left2
                        }
                    });
                    state.pendingStmts.push({
                        kind: "if",
                        label: "",
                        cond: left2,
                        thenStmt: { kind: "goto", label: "", target: skipLabel },
                        elseStmt: { kind: "goto", label: "", target: endLabel }
                    });
                    state.pendingStmts.push({ kind: "empty", label: skipLabel });
                    let right2 = lowerExpr(state, expr.right);
                    if (!isSimple(right2)) {
                        const temp = makeTempVar(state, { kind: "int" });
                        state.pendingStmts.push({
                            kind: "expr",
                            label: "",
                            expr: { kind: "assign", target: temp, value: right2 }
                        });
                        right2 = temp;
                    }
                    state.pendingStmts.push({
                        kind: "expr",
                        label: "",
                        expr: {
                            kind: "assign",
                            target: result,
                            value: right2
                        }
                    });
                    state.pendingStmts.push({ kind: "empty", label: endLabel });
                } else {
                    state.pendingStmts.push({
                        kind: "expr",
                        label: "",
                        expr: {
                            kind: "assign",
                            target: result,
                            value: left2
                        }
                    });
                    state.pendingStmts.push({
                        kind: "if",
                        label: "",
                        cond: left2,
                        thenStmt: { kind: "goto", label: "", target: endLabel },
                        elseStmt: { kind: "goto", label: "", target: skipLabel }
                    });
                    state.pendingStmts.push({ kind: "empty", label: skipLabel });
                    let right2 = lowerExpr(state, expr.right);
                    if (!isSimple(right2)) {
                        const temp = makeTempVar(state, { kind: "int" });
                        state.pendingStmts.push({
                            kind: "expr",
                            label: "",
                            expr: { kind: "assign", target: temp, value: right2 }
                        });
                        right2 = temp;
                    }
                    state.pendingStmts.push({
                        kind: "expr",
                        label: "",
                        expr: {
                            kind: "assign",
                            target: result,
                            value: right2
                        }
                    });
                    state.pendingStmts.push({ kind: "empty", label: endLabel });
                }
                return result;
            }
            let left = lowerExpr(state, expr.left);
            let right = lowerExpr(state, expr.right);
            if (!isSimple(left)) {
                const leftType = inferExprType(left, state.symbolTable);
                const temp = makeTempVar(state, leftType);
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: temp, value: left }
                });
                left = temp;
            }
            if (!isSimple(right)) {
                const rightType = inferExprType(right, state.symbolTable);
                const temp = makeTempVar(state, rightType);
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: temp, value: right }
                });
                right = temp;
            }
            return { kind: "binary", op: expr.op, left, right };
        }
        case "unary": {
            if (expr.op === "address" && expr.operand.kind === "index") {
                return lowerArrayAddress(state, expr.operand.array, expr.operand.index);
            }
            if (expr.op === "address" && expr.operand.kind === "unary" && expr.operand.op === "deref") {
                return lowerExpr(state, expr.operand.operand);
            }
            let operand = lowerExpr(state, expr.operand);
            if (expr.op === "post_inc" || expr.op === "post_dec") {
                const temp = makeTempVar(state, { kind: "int" });
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: temp, value: operand }
                });
                const op = expr.op === "post_inc" ? "add" : "sub";
                const one = { kind: "int_literal", value: 1 };
                const update = { kind: "binary", op, left: operand, right: one };
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: operand, value: update }
                });
                return temp;
            }
            if (expr.op === "pre_inc" || expr.op === "pre_dec") {
                const op = expr.op === "pre_inc" ? "add" : "sub";
                const one = { kind: "int_literal", value: 1 };
                const update = { kind: "binary", op, left: operand, right: one };
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: operand, value: update }
                });
                return operand;
            }
            if (!isSimple(operand)) {
                const operandType = inferExprType(operand, state.symbolTable);
                const temp = makeTempVar(state, operandType);
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: temp, value: operand }
                });
                operand = temp;
            }
            return { kind: "unary", op: expr.op, operand };
        }
        case "assign": {
            if (expr.target.kind === "index") {
                const ptr = lowerArrayAddress(
                    state,
                    expr.target.array,
                    expr.target.index
                );
                let value2 = lowerExpr(state, expr.value);
                if (!isSimple(value2)) {
                    const valueType = inferExprType(value2, state.symbolTable);
                    const temp = makeTempVar(state, valueType);
                    state.pendingStmts.push({
                        kind: "expr",
                        label: "",
                        expr: { kind: "assign", target: temp, value: value2 }
                    });
                    value2 = temp;
                }
                return {
                    kind: "assign",
                    target: { kind: "unary", op: "deref", operand: ptr },
                    value: value2
                };
            }
            const target = lowerExpr(state, expr.target);
            let value = lowerExpr(state, expr.value);
            if (!isSimple(value)) {
                const valueType = inferExprType(value, state.symbolTable);
                const temp = makeTempVar(state, valueType);
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: temp, value }
                });
                value = temp;
            }
            return { kind: "assign", target, value };
        }
        case "call": {
            const func = lowerExpr(state, expr.func);
            const args = [];
            for (const arg of expr.args) {
                let loweredArg = lowerExpr(state, arg);
                if (!isSimple(loweredArg)) {
                    const argType = inferExprType(loweredArg, state.symbolTable);
                    const temp = makeTempVar(state, argType);
                    state.pendingStmts.push({
                        kind: "expr",
                        label: "",
                        expr: { kind: "assign", target: temp, value: loweredArg }
                    });
                    loweredArg = temp;
                }
                args.push(loweredArg);
            }
            return { kind: "call", func, args };
        }
        case "index": {
            const ptr = lowerArrayAddress(state, expr.array, expr.index);
            return { kind: "unary", op: "deref", operand: ptr };
        }
    }
}
function normalizeExpr(expr) {
    switch (expr.kind) {
        case "int_literal":
        case "identifier":
            return expr;
        case "binary":
            return {
                kind: "binary",
                op: expr.op,
                left: normalizeExpr(expr.left),
                right: normalizeExpr(expr.right)
            };
        case "unary":
            return {
                kind: "unary",
                op: expr.op,
                operand: normalizeExpr(expr.operand)
            };
        case "assign":
            return {
                kind: "assign",
                target: normalizeExpr(expr.target),
                value: normalizeExpr(expr.value)
            };
        case "call":
            return {
                kind: "call",
                func: normalizeExpr(expr.func),
                args: expr.args.map(normalizeExpr)
            };
        case "index":
            return {
                kind: "index",
                array: normalizeExpr(expr.array),
                index: normalizeExpr(expr.index)
            };
    }
}
function normalizeStmt(stmt, state) {
    switch (stmt.kind) {
        case "expr":
            return [
                {
                    kind: "expr",
                    label: stmt.label,
                    expr: normalizeExpr(stmt.expr)
                }
            ];
        case "return":
            return [
                {
                    kind: "return",
                    label: stmt.label,
                    value: stmt.value ? normalizeExpr(stmt.value) : null
                }
            ];
        case "goto":
        case "empty":
            return [stmt];
        case "decl":
            if (stmt.decl.kind === "var" && stmt.decl.init) {
                return [
                    {
                        kind: "decl",
                        label: stmt.label,
                        decl: {
                            ...stmt.decl,
                            init: normalizeExpr(stmt.decl.init)
                        }
                    }
                ];
            }
            return [stmt];
        case "block": {
            const result = [];
            for (const s of stmt.stmts) {
                result.push(...normalizeStmt(s, state));
            }
            if (stmt.label && result.length > 0) {
                result[0] = { ...result[0], label: stmt.label };
            }
            return result;
        }
        case "if": {
            const result = [];
            const thenLabel = genLabel(state, "then");
            const elseLabel = genLabel(state, "else");
            const endLabel = genLabel(state, "endif");
            if (stmt.label) {
                result.push({ kind: "empty", label: stmt.label });
            }
            const cond = normalizeExpr(stmt.cond);
            result.push({
                kind: "if",
                label: "",
                cond,
                thenStmt: { kind: "goto", label: "", target: thenLabel },
                elseStmt: { kind: "goto", label: "", target: elseLabel }
            });
            result.push({ kind: "empty", label: thenLabel });
            result.push(...normalizeStmt(stmt.thenStmt, state));
            result.push({ kind: "goto", label: "", target: endLabel });
            result.push({ kind: "empty", label: elseLabel });
            if (stmt.elseStmt) {
                result.push(...normalizeStmt(stmt.elseStmt, state));
            }
            result.push({ kind: "empty", label: endLabel });
            return result;
        }
        case "while": {
            const result = [];
            const loopLabel = genLabel(state, "loop");
            const bodyLabel = genLabel(state, "body");
            const endLabel = genLabel(state, "end");
            if (stmt.label) {
                result.push({ kind: "empty", label: stmt.label });
            }
            result.push({ kind: "empty", label: loopLabel });
            const cond = normalizeExpr(stmt.cond);
            result.push({
                kind: "if",
                label: "",
                cond,
                thenStmt: { kind: "goto", label: "", target: bodyLabel },
                elseStmt: { kind: "goto", label: "", target: endLabel }
            });
            result.push({ kind: "empty", label: bodyLabel });
            result.push(...normalizeStmt(stmt.body, state));
            result.push({ kind: "goto", label: "", target: loopLabel });
            result.push({ kind: "empty", label: endLabel });
            return result;
        }
        case "for": {
            const result = [];
            const loopLabel = genLabel(state, "loop");
            const bodyLabel = genLabel(state, "body");
            const endLabel = genLabel(state, "end");
            if (stmt.label) {
                result.push({ kind: "empty", label: stmt.label });
            }
            if (stmt.init) {
                result.push(...normalizeStmt(stmt.init, state));
            }
            result.push({ kind: "empty", label: loopLabel });
            if (stmt.cond) {
                const cond = normalizeExpr(stmt.cond);
                result.push({
                    kind: "if",
                    label: "",
                    cond,
                    thenStmt: { kind: "goto", label: "", target: bodyLabel },
                    elseStmt: { kind: "goto", label: "", target: endLabel }
                });
            } else {
                result.push({ kind: "goto", label: "", target: bodyLabel });
            }
            result.push({ kind: "empty", label: bodyLabel });
            result.push(...normalizeStmt(stmt.body, state));
            if (stmt.update) {
                result.push({
                    kind: "expr",
                    label: "",
                    expr: normalizeExpr(stmt.update)
                });
            }
            result.push({ kind: "goto", label: "", target: loopLabel });
            result.push({ kind: "empty", label: endLabel });
            return result;
        }
    }
}
function normalizeFunction(decl, state) {
    if (decl.kind !== "func" || !decl.body) {
        return decl;
    }
    const normalizedStmts = normalizeStmt(decl.body, state);
    return {
        ...decl,
        body: {
            kind: "block",
            label: "",
            stmts: normalizedStmts
        }
    };
}
function genTempLowerer(state) {
    return `_t${state.tempCounter++}`;
}
function makeTempVar(state, type) {
    const name = genTempLowerer(state);
    const decl = { kind: "var", name, type, init: null };
    state.pendingStmts.push({
        kind: "decl",
        label: "",
        decl
    });
    return { kind: "identifier", name };
}
function isSimple(expr) {
    return expr.kind === "int_literal" || expr.kind === "identifier";
}
function lowerStmt(state, stmt) {
    state.pendingStmts = [];
    switch (stmt.kind) {
        case "expr": {
            const expr = lowerExpr(state, stmt.expr);
            const result = [...state.pendingStmts];
            if (expr.kind === "assign" || expr.kind === "call") {
                result.push({ kind: "expr", label: stmt.label, expr });
            } else if (result.length > 0) {
                result[0] = { ...result[0], label: stmt.label };
            }
            return result;
        }
        case "return": {
            if (!stmt.value) {
                return [stmt];
            }
            let value = lowerExpr(state, stmt.value);
            if (!isSimple(value)) {
                const temp = makeTempVar(state, { kind: "int" });
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: temp, value }
                });
                value = temp;
            }
            const result = [...state.pendingStmts];
            result.push({ kind: "return", label: stmt.label, value });
            return result;
        }
        case "if": {
            let cond = lowerExpr(state, stmt.cond);
            if (!isSimple(cond)) {
                const temp = makeTempVar(state, { kind: "int" });
                state.pendingStmts.push({
                    kind: "expr",
                    label: "",
                    expr: { kind: "assign", target: temp, value: cond }
                });
                cond = temp;
            }
            const result = [...state.pendingStmts];
            result.push({
                kind: "if",
                label: stmt.label,
                cond,
                thenStmt: stmt.thenStmt,
                elseStmt: stmt.elseStmt
            });
            return result;
        }
        case "decl": {
            if (stmt.decl.kind === "var" && stmt.decl.init) {
                let init2 = lowerExpr(state, stmt.decl.init);
                if (!isSimple(init2)) {
                    const temp = makeTempVar(state, stmt.decl.type);
                    state.pendingStmts.push({
                        kind: "expr",
                        label: "",
                        expr: { kind: "assign", target: temp, value: init2 }
                    });
                    init2 = temp;
                }
                const result = [...state.pendingStmts];
                result.push({
                    kind: "decl",
                    label: stmt.label,
                    decl: { ...stmt.decl, init: init2 }
                });
                return result;
            }
            return [stmt];
        }
        case "block": {
            const result = [];
            let firstStmt = true;
            for (const s of stmt.stmts) {
                const lowered = lowerStmt(state, s);
                if (firstStmt && stmt.label && lowered.length > 0) {
                    lowered[0] = { ...lowered[0], label: stmt.label };
                    firstStmt = false;
                }
                result.push(...lowered);
            }
            return result;
        }
        default:
            return [stmt];
    }
}
function lowerFunctionExpressions(decl, state) {
    if (decl.kind !== "func" || !decl.body) {
        return decl;
    }
    const loweredStmts = lowerStmt(state, decl.body);
    return {
        ...decl,
        body: {
            kind: "block",
            label: "",
            stmts: loweredStmts
        }
    };
}
function normalizeProgram(program, symbolTable) {
    const cfState = createCFNormalizer();
    const exprState = createExprLowerer(symbolTable);
    const normalizedDecls = [];
    for (const decl of program.declarations) {
        let normalized = normalizeFunction(decl, cfState);
        normalized = lowerFunctionExpressions(normalized, exprState);
        normalizedDecls.push(normalized);
    }
    const result = { declarations: normalizedDecls };
    const normalizedAstSymbolTable = semantic_analyze(result).symbolTable;
    return { program: result, symbolTable: normalizedAstSymbolTable };
}

// src/liveness.ts
var ENTRY_POSITION = -2;
var TERM_POSITION = -1;
function computeReversePostorder(func, cfg) {
    const visited = /* @__PURE__ */ new Set();
    const postorder = [];
    function dfs(label) {
        if (visited.has(label)) return;
        visited.add(label);
        const succs = cfg.successors.get(label);
        if (succs) {
            for (const succ of succs) {
                dfs(succ);
            }
        }
        postorder.push(label);
    }
    dfs(func.entryBlock);
    return postorder.reverse();
}
function computeLiveRanges(func) {
    const ranges = /* @__PURE__ */ new Map();
    const vregObjects = /* @__PURE__ */ new Map();
    const firstDef = /* @__PURE__ */ new Map();
    const lastUse = /* @__PURE__ */ new Map();
    const useCount = /* @__PURE__ */ new Map();
    const defCount = /* @__PURE__ */ new Map();
    for (const param of func.params) {
        vregObjects.set(param.id, param);
        firstDef.set(param.id, { block: func.entryBlock, index: ENTRY_POSITION });
    }
    for (const [label, block] of func.blocks) {
        for (let i = 0; i < block.instructions.length; i++) {
            const inst = block.instructions[i];
            const pos = { block: label, index: i };
            const def = instGetDef(inst);
            if (def) {
                vregObjects.set(def.id, def);
                if (!firstDef.has(def.id)) {
                    firstDef.set(def.id, pos);
                }
                defCount.set(def.id, (defCount.get(def.id) || 0) + 1);
            }
            for (const use of instGetUses(inst)) {
                vregObjects.set(use.id, use);
                lastUse.set(use.id, pos);
                useCount.set(use.id, (useCount.get(use.id) || 0) + 1);
            }
        }
        const termPos = { block: label, index: TERM_POSITION };
        for (const use of termGetUses(block.terminator)) {
            vregObjects.set(use.id, use);
            lastUse.set(use.id, termPos);
            useCount.set(use.id, (useCount.get(use.id) || 0) + 1);
        }
    }
    for (const [vregId, vregObj] of vregObjects) {
        const def = firstDef.get(vregId);
        const use = lastUse.get(vregId);
        if (!def && !use) {
            continue;
        }
        const start = def || use;
        const end = use || def;
        ranges.set(vregId, {
            vreg: vregObj,
            start,
            end,
            useCount: useCount.get(vregId) || 0,
            defCount: defCount.get(vregId) || 0
        });
    }
    return ranges;
}
function computeLiveness(func, cfg) {
    const liveIn = /* @__PURE__ */ new Map();
    const liveOut = /* @__PURE__ */ new Map();
    for (const label of func.blocks.keys()) {
        liveIn.set(label, /* @__PURE__ */ new Set());
        liveOut.set(label, /* @__PURE__ */ new Set());
    }
    let changed = true;
    while (changed) {
        changed = false;
        const labels = computeReversePostorder(func, cfg);
        for (const label of labels) {
            const block = func.blocks.get(label);
            const oldLiveIn = new Set(liveIn.get(label));
            const out = /* @__PURE__ */ new Set();
            const successors = cfg.successors.get(label) || /* @__PURE__ */ new Set();
            for (const succ of successors) {
                const succLiveIn = liveIn.get(succ);
                for (const vreg of succLiveIn) {
                    out.add(vreg);
                }
            }
            liveOut.set(label, out);
            const in_ = new Set(out);
            for (const use of termGetUses(block.terminator)) {
                in_.add(use);
            }
            for (let i = block.instructions.length - 1; i >= 0; i--) {
                const inst = block.instructions[i];
                const def = instGetDef(inst);
                if (def) {
                    in_.delete(def);
                }
                for (const use of instGetUses(inst)) {
                    if (use.kind !== "addr") {
                        in_.add(use);
                    }
                }
            }
            liveIn.set(label, in_);
            if (!setsEqual(oldLiveIn, in_)) {
                changed = true;
            }
        }
    }
    const liveRanges = computeLiveRanges(func);
    return { liveIn, liveOut, liveRanges };
}

// src/ssa.ts
function vregIsSSAEligible(vreg) {
    const yes = vreg.kind === "var" || vreg.kind === "param";
    return yes;
}
function vregSourceName(vreg) {
    if (vreg.kind === "var" || vreg.kind === "param" || vreg.kind === "addr") {
        return vreg.sourceName;
    }
    return void 0;
}
function computeDominance(func, cfg) {
    const idom = computeImmediateDominators(func, cfg);
    const domTree = buildDominatorTree(idom);
    const domFrontier = computeDominanceFrontiers(func, cfg, idom);
    return { idom, domTree, domFrontier };
}
function dominates(entry, dom, node, cfg) {
    if (dom === node) return true;
    if (dom === entry) return true;
    if (node === entry) return false;
    const visited = /* @__PURE__ */ new Set();
    const stack = [entry];
    let foundPath = false;
    while (stack.length > 0) {
        const current = stack.pop();
        if (visited.has(current)) continue;
        visited.add(current);
        if (current === dom) continue;
        if (current === node) {
            foundPath = true;
            break;
        }
        const succs = cfg.successors.get(current) || /* @__PURE__ */ new Set();
        for (const succ of succs) {
            stack.push(succ);
        }
    }
    const dominatesResult = !foundPath;
    return dominatesResult;
}
function computeImmediateDominators(func, cfg) {
    const idom = /* @__PURE__ */ new Map();
    const labels = Array.from(func.blocks.keys()).sort((a, b) => a - b);
    for (const label of labels) {
        if (label === func.entryBlock) continue;
        const dominators = /* @__PURE__ */ new Set();
        for (const candidate of labels) {
            if (dominates(func.entryBlock, candidate, label, cfg)) {
                dominators.add(candidate);
            }
        }
        for (const dom of dominators) {
            let isImmediate = true;
            for (const other of dominators) {
                if (other !== dom && other !== label && dominates(dom, other, label, cfg)) {
                    isImmediate = false;
                    break;
                }
            }
            if (isImmediate && dom !== label) {
                idom.set(label, dom);
                break;
            }
        }
    }
    return idom;
}
function buildDominatorTree(idom) {
    const domTree = /* @__PURE__ */ new Map();
    for (const [node, parent] of idom) {
        if (!domTree.has(parent)) {
            domTree.set(parent, /* @__PURE__ */ new Set());
        }
        domTree.get(parent).add(node);
    }
    return domTree;
}
function computeDominanceFrontiers(func, cfg, idom) {
    const df = /* @__PURE__ */ new Map();
    for (const label of func.blocks.keys()) {
        df.set(label, /* @__PURE__ */ new Set());
    }
    for (const [label, block] of func.blocks) {
        const preds = cfg.predecessors.get(label) || /* @__PURE__ */ new Set();
        if (preds.size >= 2) {
            for (const pred of preds) {
                let runner = pred;
                while (runner !== idom.get(label)) {
                    df.get(runner).add(label);
                    const nextRunner = idom.get(runner);
                    if (nextRunner === void 0 && runner !== func.entryBlock) {
                        break;
                    }
                    runner = nextRunner;
                    if (runner === void 0) break;
                }
            }
        }
    }
    return df;
}
function createTempVReg(func) {
    const temp = {
        kind: "temp",
        id: 1e5 + tempCounter++
    };
    return temp;
}
function renameVariables(func, domInfo, cfg) {
    const state = {
        stacks: /* @__PURE__ */ new Map(),
        counter: /* @__PURE__ */ new Map(),
        nextId: 1e3
    };
    for (const varName of func.ssaInfo.variables) {
        if (!func.ssaInfo.addressed.has(varName)) {
            state.stacks.set(varName, []);
            state.counter.set(varName, 0);
            const undefVReg = {
                kind: "var",
                id: state.nextId++,
                sourceName: varName,
                version: 0
            };
            const isParam = func.params.some((p) => vregSourceName(p) === varName);
            if (!isParam) {
                state.stacks.get(varName).push(undefVReg);
                state.counter.set(varName, 1);
            }
        }
    }
    for (const param of func.params) {
        if (vregIsSSAEligible(param)) {
            const sourceName = vregSourceName(param);
            state.stacks.set(sourceName, []);
            state.counter.set(sourceName, 0);
            state.stacks.get(sourceName).push(param);
            state.counter.set(sourceName, 1);
        }
    }
    renameBlock(func.entryBlock, func, domInfo, cfg, state);
}
function insertPhis(func, domInfo, cfg) {
    const liveness = computeLiveness(func, cfg);
    const liveVariables = /* @__PURE__ */ new Set();
    for (const [label, liveSet] of liveness.liveIn) {
        for (const vreg of liveSet) {
            const name = vregSourceName(vreg);
            if (name) {
                liveVariables.add(name);
            }
        }
    }
    for (const [label, liveSet] of liveness.liveOut) {
        for (const vreg of liveSet) {
            const name = vregSourceName(vreg);
            if (name) {
                liveVariables.add(name);
            }
        }
    }
    const phiPlacements = /* @__PURE__ */ new Map();
    for (const varName of func.ssaInfo.variables) {
        if (func.ssaInfo.addressed.has(varName)) {
            continue;
        }
        if (!liveVariables.has(varName)) {
            continue;
        }
        const defBlocks = func.ssaInfo.definitions.get(varName);
        if (!defBlocks || defBlocks.size === 0) {
            continue;
        }
        const worklist = Array.from(defBlocks);
        const processed = /* @__PURE__ */ new Set();
        while (worklist.length > 0) {
            const block = worklist.pop();
            const df = domInfo.domFrontier.get(block) || /* @__PURE__ */ new Set();
            for (const frontierBlock of df) {
                if (processed.has(frontierBlock)) {
                    continue;
                }
                const blockLiveIn = liveness.liveIn.get(frontierBlock);
                const isLiveHere = blockLiveIn && Array.from(blockLiveIn).some(
                    (vreg) => vregSourceName(vreg) === varName
                );
                if (!isLiveHere) {
                    continue;
                }
                if (!phiPlacements.has(frontierBlock)) {
                    phiPlacements.set(frontierBlock, /* @__PURE__ */ new Set());
                }
                phiPlacements.get(frontierBlock).add(varName);
                processed.add(frontierBlock);
                worklist.push(frontierBlock);
            }
        }
    }
    for (const [blockLabel, varNames] of phiPlacements) {
        const block = func.blocks.get(blockLabel);
        const preds = Array.from(cfg.predecessors.get(blockLabel) || []);
        for (const varName of varNames) {
            const phiDst = {
                kind: "var",
                id: -1,
                sourceName: varName,
                version: -1
            };
            const phiInst = {
                op: "phi",
                dst: phiDst,
                sources: preds.map((pred) => ({
                    pred,
                    vreg: phiDst
                }))
            };
            block.instructions.unshift(phiInst);
        }
    }
    return phiPlacements;
}
function renameBlock(label, func, domInfo, cfg, state) {
    const block = func.blocks.get(label);
    const pushedVars = [];
    for (const inst of block.instructions) {
        if (inst.op !== "phi") break;
        const sourceName = vregSourceName(inst.dst);
        if (!sourceName) continue;
        const newVReg = newVersion(sourceName, state);
        inst.dst = newVReg;
        pushedVars.push({ name: sourceName, vreg: newVReg });
    }
    for (const inst of block.instructions) {
        if (inst.op === "phi") continue;
        rewriteInstructionUses(inst, state);
        const def = instGetDef(inst);
        if (def) {
            const sourceName = vregSourceName(def);
            if (sourceName && vregIsSSAEligible(def)) {
                const newVReg = newVersion(sourceName, state);
                updateInstDef(inst, newVReg);
                pushedVars.push({ name: sourceName, vreg: newVReg });
            }
        }
    }
    rewriteTerminatorUses(block.terminator, state);
    const succs = cfg.successors.get(label) || /* @__PURE__ */ new Set();
    for (const succ of succs) {
        const succBlock = func.blocks.get(succ);
        const preds = Array.from(cfg.predecessors.get(succ));
        const predIndex = preds.indexOf(label);
        if (predIndex === -1) {
            continue;
        }
        for (const inst of succBlock.instructions) {
            if (inst.op !== "phi") break;
            const sourceName = vregSourceName(inst.dst);
            if (!sourceName) continue;
            const currentVReg = currentVersion(sourceName, state);
            if (currentVReg) {
                inst.sources[predIndex].vreg = currentVReg;
            }
        }
    }
    const children = domInfo.domTree.get(label) || /* @__PURE__ */ new Set();
    for (const child of children) {
        renameBlock(child, func, domInfo, cfg, state);
    }
}
function newVersion(varName, state) {
    const version = state.counter.get(varName);
    state.counter.set(varName, version + 1);
    const vreg = {
        kind: "var",
        id: state.nextId++,
        sourceName: varName,
        version
    };
    state.stacks.get(varName).push(vreg);
    return vreg;
}
function currentVersion(varName, state) {
    const stack = state.stacks.get(varName);
    if (!stack || stack.length === 0) {
        return null;
    }
    return stack[stack.length - 1];
}
function rewriteInstructionUses(inst, state) {
    switch (inst.op) {
        case "phi":
            break;
        case "mov":
            inst.src = rewriteVReg(inst.src, state);
            break;
        case "add":
        case "sub":
        case "mul":
        case "div":
        case "mod":
            inst.lhs = rewriteVReg(inst.lhs, state);
            inst.rhs = rewriteVReg(inst.rhs, state);
            break;
        case "cmp":
            inst.lhs = rewriteVReg(inst.lhs, state);
            inst.rhs = rewriteVReg(inst.rhs, state);
            break;
        case "load":
            inst.ptr = rewriteVReg(inst.ptr, state);
            break;
        case "store":
            inst.ptr = rewriteVReg(inst.ptr, state);
            inst.value = rewriteVReg(inst.value, state);
            break;
        case "call":
            inst.args = inst.args.map((arg) => rewriteVReg(arg, state));
            break;
        case "loadi":
        case "alloca":
            break;
    }
}
function rewriteTerminatorUses(term, state) {
    switch (term.op) {
        case "branch":
            term.cond = rewriteVReg(term.cond, state);
            break;
        case "ret":
            if (term.value) {
                term.value = rewriteVReg(term.value, state);
            }
            break;
        case "jump":
            break;
    }
}
function rewriteVReg(vreg, state) {
    if (!vregIsSSAEligible(vreg)) {
        return vreg;
    }
    const sourceName = vregSourceName(vreg);
    if (!sourceName) {
        return vreg;
    }
    const current = currentVersion(sourceName, state);
    if (!current) {
        return vreg;
    }
    return current;
}
function constructSSA(program) {
    for (const func of program.functions) {
        const cfg = program.cfg.functionCFGs.get(func.name);
        const domInfo = computeDominance(func, cfg);
        insertPhis(func, domInfo, cfg);
        renameVariables(func, domInfo, cfg);
    }
}
function destructSSA(program) {
    for (const func of program.functions) {
        destructSSAInFunction(func, program.cfg.functionCFGs.get(func.name));
    }
}
function destructSSAInFunction(func, cfg) {
    const phisToRemove = [];
    for (const [label, block] of func.blocks) {
        for (const inst of block.instructions) {
            if (inst.op !== "phi") break;
            phisToRemove.push({ block: label, phi: inst });
        }
    }
    const needsSplit = /* @__PURE__ */ new Map();
    let nextLabel = Math.max(...func.blocks.keys()) + 1;
    for (const { block: blockLabel, phi } of phisToRemove) {
        const targetBlock = func.blocks.get(blockLabel);
        const preds = cfg.predecessors.get(blockLabel) || /* @__PURE__ */ new Set();
        for (const source of phi.sources) {
            const pred = source.pred;
            const predBlock = func.blocks.get(pred);
            const predSuccs = cfg.successors.get(pred) || /* @__PURE__ */ new Set();
            if (predSuccs.size > 1 && preds.size > 1) {
                const edgeKey = `${pred}->${blockLabel}`;
                if (!needsSplit.has(edgeKey)) {
                    needsSplit.set(edgeKey, nextLabel++);
                }
            }
        }
    }
    for (const [edgeKey, newLabel] of needsSplit) {
        const [predStr, targetStr] = edgeKey.split("->");
        const pred = parseInt(predStr);
        const target = parseInt(targetStr);
        const newBlock = {
            label: newLabel,
            instructions: [],
            terminator: { op: "jump", target }
        };
        func.blocks.set(newLabel, newBlock);
        const predBlock = func.blocks.get(pred);
        const term = predBlock.terminator;
        if (term.op === "branch") {
            if (term.trueTarget === target) term.trueTarget = newLabel;
            if (term.falseTarget === target) term.falseTarget = newLabel;
        } else if (term.op === "jump" && term.target === target) {
            term.target = newLabel;
        }
        cfg.successors.get(pred).delete(target);
        cfg.successors.get(pred).add(newLabel);
        cfg.successors.set(newLabel, /* @__PURE__ */ new Set([target]));
        cfg.predecessors.get(target).delete(pred);
        cfg.predecessors.get(target).add(newLabel);
        cfg.predecessors.set(newLabel, /* @__PURE__ */ new Set([pred]));
        const targetBlock = func.blocks.get(target);
        for (const inst of targetBlock.instructions) {
            if (inst.op !== "phi") break;
            for (const src of inst.sources) {
                if (src.pred === pred) {
                    src.pred = newLabel;
                }
            }
        }
    }
    const copiesByPred = /* @__PURE__ */ new Map();
    for (const { block: blockLabel, phi } of phisToRemove) {
        for (const source of phi.sources) {
            const pred = source.pred;
            const srcVreg = source.vreg;
            const dstVreg = phi.dst;
            if (vregEqual(srcVreg, dstVreg)) continue;
            if (!copiesByPred.has(pred)) {
                copiesByPred.set(pred, []);
            }
            copiesByPred.get(pred).push({ dst: dstVreg, src: srcVreg });
        }
    }
    for (const [pred, copies] of copiesByPred) {
        insertCopiesAtEnd(func.blocks.get(pred), copies, func);
    }
    for (const [label, block] of func.blocks) {
        block.instructions = block.instructions.filter((inst) => inst.op !== "phi");
    }
}
function sequentializeParallelCopies(copies, func) {
    if (copies.length === 0) return [];
    if (copies.length === 1) return [{ ...copies[0], isTemp: false }];
    const result = [];
    let remaining = new Set(copies);
    const processed = /* @__PURE__ */ new Set();
    function isDefinedBy(vreg) {
        for (const copy of remaining) {
            if (vregEqual(copy.dst, vreg)) return copy;
        }
        return void 0;
    }
    while (remaining.size > 0) {
        let madeProgress = false;
        for (const copy of remaining) {
            const srcDef = isDefinedBy(copy.src);
            const canProcess = !srcDef || processed.has(copy.src.id);
            if (canProcess) {
                result.push({ ...copy, isTemp: false });
                remaining.delete(copy);
                processed.add(copy.dst.id);
                madeProgress = true;
                break;
            }
        }
        if (!madeProgress && remaining.size > 0) {
            const cycleStart = remaining.values().next().value;
            const temp = createTempVReg(func);
            result.push({ dst: temp, src: cycleStart.src, isTemp: true });
            processed.add(cycleStart.src.id);
            const updated = /* @__PURE__ */ new Set();
            for (const copy of remaining) {
                if (vregEqual(copy.src, cycleStart.src)) {
                    updated.add({ dst: copy.dst, src: temp });
                } else {
                    updated.add(copy);
                }
            }
            remaining = updated;
            madeProgress = true;
        }
        if (!madeProgress) {
            break;
        }
    }
    return result;
}
var tempCounter = 0;
function insertCopiesAtEnd(block, copies, func) {
    if (copies.length === 0) {
        return;
    }
    const nonIdentityCopies = copies.filter(
        (copy) => !vregEqual(copy.dst, copy.src)
    );
    if (nonIdentityCopies.length === 0) {
        return;
    }
    const sequenced = sequentializeParallelCopies(nonIdentityCopies, func);
    for (const copy of sequenced) {
        const moveInst = {
            op: "mov",
            dst: copy.dst,
            src: copy.src
        };
        block.instructions.push(moveInst);
    }
}

// src/optimizations.ts
function optimizeSSA(program) {
    for (const func of program.functions) {
        optimizeFunction(func, program);
    }
}
function findConstantValue(vreg, func) {
    for (const block of func.blocks.values()) {
        for (const inst of block.instructions) {
            if (inst.op === "loadi" && vregEqual(inst.dst, vreg)) {
                return inst.imm;
            }
        }
    }
    return null;
}
function isConstantZero(vreg, func) {
    const value = findConstantValue(vreg, func);
    return value === 0;
}
function isConstantOne(vreg, func) {
    const value = findConstantValue(vreg, func);
    return value === 1;
}
function algebraicSimplification(func) {
    let changed = false;
    for (const [label, block] of func.blocks) {
        for (let i = 0; i < block.instructions.length; i++) {
            const inst = block.instructions[i];
            if (inst.op !== "add" && inst.op !== "sub" && inst.op !== "mul" && inst.op !== "div") {
                continue;
            }
            const simplified = simplifyInstruction(inst, func);
            if (simplified) {
                block.instructions[i] = simplified;
                changed = true;
            }
        }
    }
    return changed;
}
function mergeBranches(ir) {
    let changed = false;
    for (const [label, block] of ir.blocks) {
        if (block.terminator.op != "branch") {
            continue;
        }
        if (block.terminator.falseTarget === block.terminator.trueTarget) {
            block.terminator = { op: "jump", target: block.terminator.falseTarget };
            changed = true;
        }
    }
    return changed;
}
function simplifyInstruction(inst, func) {
    switch (inst.op) {
        case "add":
            if (isConstantZero(inst.rhs, func)) {
                return {
                    op: "mov",
                    dst: inst.dst,
                    src: inst.lhs
                };
            }
            if (isConstantZero(inst.lhs, func)) {
                return {
                    op: "mov",
                    dst: inst.dst,
                    src: inst.rhs
                };
            }
            break;
        case "sub":
            if (isConstantZero(inst.rhs, func)) {
                return {
                    op: "mov",
                    dst: inst.dst,
                    src: inst.lhs
                };
            }
            if (vregEqual(inst.lhs, inst.rhs)) {
                return {
                    op: "loadi",
                    dst: inst.dst,
                    imm: 0
                };
            }
            break;
        case "mul":
            if (isConstantOne(inst.rhs, func)) {
                return {
                    op: "mov",
                    dst: inst.dst,
                    src: inst.lhs
                };
            }
            if (isConstantOne(inst.lhs, func)) {
                return {
                    op: "mov",
                    dst: inst.dst,
                    src: inst.rhs
                };
            }
            if (isConstantZero(inst.rhs, func)) {
                return {
                    op: "loadi",
                    dst: inst.dst,
                    imm: 0
                };
            }
            if (isConstantZero(inst.lhs, func)) {
                return {
                    op: "loadi",
                    dst: inst.dst,
                    imm: 0
                };
            }
            break;
        case "div":
            if (isConstantOne(inst.rhs, func)) {
                return {
                    op: "mov",
                    dst: inst.dst,
                    src: inst.lhs
                };
            }
            break;
    }
    return null;
}
function optimizeFunction(func, program) {
    let changed = true;
    let iterations = 0;
    const MAX_ITERATIONS = 20;
    let cfg = program.cfg.functionCFGs.get(func.name);
    let domInfo = computeDominance(func, cfg);
    while (changed && iterations < MAX_ITERATIONS) {
        changed = false;
        iterations++;
        if (sparseConditionalConstantPropagation(func, cfg)) {
            changed = true;
            updateCFG(func, cfg);
            domInfo = computeDominance(func, cfg);
        }
        if (algebraicSimplification(func)) {
            changed = true;
        }
        if (copyPropagation(func, domInfo, cfg)) {
            changed = true;
        }
        if (eliminateDeadCode(func)) {
            changed = true;
            updateCFG(func, cfg);
            domInfo = computeDominance(func, cfg);
        }
        if (mergeBranches(func)) {
            changed = true;
            updateCFG(func, cfg);
            domInfo = computeDominance(func, cfg);
        }
    }
}
function blockDominates(dom, node, domInfo) {
    if (dom === node) return true;
    let current = node;
    while (current !== void 0) {
        if (current === dom) return true;
        current = domInfo.idom.get(current);
    }
    return false;
}
function findCopies(func) {
    const copies = [];
    for (const [label, block] of func.blocks) {
        for (const inst of block.instructions) {
            if (inst.op === "mov") {
                if (!vregEqual(inst.dst, inst.src)) {
                    copies.push({
                        dst: inst.dst,
                        src: inst.src,
                        definingBlock: label,
                        isTrivialPhi: false
                    });
                }
            } else if (inst.op === "phi") {
                const firstSrc = inst.sources[0]?.vreg;
                if (firstSrc && inst.sources.every((s) => vregEqual(s.vreg, firstSrc))) {
                    if (vregIsMemoryLocation(firstSrc)) {
                        continue;
                    }
                    copies.push({
                        dst: inst.dst,
                        src: firstSrc,
                        definingBlock: label,
                        isTrivialPhi: true
                    });
                }
            }
        }
    }
    return copies;
}
function canReplacePhi(copy, phiBlock, phiSourcePred, domInfo) {
    if (copy.definingBlock === phiSourcePred) {
        return true;
    }
    return blockDominates(copy.definingBlock, phiSourcePred, domInfo);
}
function replaceUsesInInst(inst, from, to) {
    let changed = false;
    switch (inst.op) {
        case "mov":
            if (vregEqual(inst.src, from)) {
                inst.src = to;
                changed = true;
            }
            break;
        case "add":
        case "sub":
        case "mul":
        case "div":
        case "mod":
            if (vregEqual(inst.lhs, from)) {
                inst.lhs = to;
                changed = true;
            }
            if (vregEqual(inst.rhs, from)) {
                inst.rhs = to;
                changed = true;
            }
            break;
        case "cmp":
            if (vregEqual(inst.lhs, from)) {
                inst.lhs = to;
                changed = true;
            }
            if (vregEqual(inst.rhs, from)) {
                inst.rhs = to;
                changed = true;
            }
            break;
        case "store":
            if (vregEqual(inst.ptr, from)) {
                inst.ptr = to;
                changed = true;
            }
            if (vregEqual(inst.value, from)) {
                inst.value = to;
                changed = true;
            }
            break;
        case "load":
            if (vregEqual(inst.ptr, from)) {
                inst.ptr = to;
                changed = true;
            }
            break;
        case "call":
            for (let i = 0; i < inst.args.length; i++) {
                if (vregEqual(inst.args[i], from)) {
                    inst.args[i] = to;
                    changed = true;
                }
            }
            break;
        case "loadi":
        case "alloca":
        case "phi":
            break;
    }
    return changed;
}
function replaceUsesInTerminator(term, from, to) {
    let changed = false;
    if (term.op === "branch" && vregEqual(term.cond, from)) {
        term.cond = to;
        changed = true;
    } else if (term.op === "ret" && term.value && vregEqual(term.value, from)) {
        term.value = to;
        changed = true;
    }
    return changed;
}
function propagateCopy(func, copy, domInfo, cfg) {
    let changed = false;
    for (const [label, block] of func.blocks) {
        for (const inst of block.instructions) {
            if (inst.op === "phi") {
                for (const source of inst.sources) {
                    if (vregEqual(source.vreg, copy.dst)) {
                        if (canReplacePhi(copy, label, source.pred, domInfo)) {
                            source.vreg = copy.src;
                            changed = true;
                        }
                    }
                }
            } else {
                if (!blockDominates(copy.definingBlock, label, domInfo)) {
                    continue;
                }
                if (replaceUsesInInst(inst, copy.dst, copy.src)) {
                    changed = true;
                }
            }
        }
        if (blockDominates(copy.definingBlock, label, domInfo)) {
            if (replaceUsesInTerminator(block.terminator, copy.dst, copy.src)) {
                changed = true;
            }
        }
    }
    return changed;
}
function copyPropagation(func, domInfo, cfg) {
    const copies = findCopies(func);
    if (copies.length === 0) {
        return false;
    }
    let changed = false;
    for (const copy of copies) {
        if (propagateCopy(func, copy, domInfo, cfg)) {
            changed = true;
        }
    }
    return changed;
}
function isCritical(inst) {
    return inst.op === "store" || inst.op === "call" || inst.op === "alloca";
}
function findDefiningInstruction(vreg, func) {
    for (const block of func.blocks.values()) {
        for (const inst of block.instructions) {
            const def = instGetDef(inst);
            if (def && vregEqual(def, vreg)) {
                return inst;
            }
        }
    }
    return void 0;
}
function eliminateDeadCode(func) {
    const neededVRegs = /* @__PURE__ */ new Set();
    const neededInsts = /* @__PURE__ */ new Set();
    const worklist = [];
    const markVReg = (vreg) => {
        if (!neededVRegs.has(vreg.id)) {
            neededVRegs.add(vreg.id);
            worklist.push(vreg);
        }
    };
    for (const block of func.blocks.values()) {
        for (const inst of block.instructions) {
            if (isCritical(inst)) {
                neededInsts.add(inst);
                for (const use of instGetUses(inst)) {
                    markVReg(use);
                }
            }
        }
        for (const use of termGetUses(block.terminator)) {
            markVReg(use);
        }
    }
    while (worklist.length > 0) {
        const vreg = worklist.pop();
        const definingInst = findDefiningInstruction(vreg, func);
        if (definingInst && !neededInsts.has(definingInst)) {
            neededInsts.add(definingInst);
            for (const use of instGetUses(definingInst)) {
                markVReg(use);
            }
        }
    }
    let changed = false;
    for (const block of func.blocks.values()) {
        const originalLength = block.instructions.length;
        block.instructions = block.instructions.filter(
            (inst) => neededInsts.has(inst)
        );
        if (block.instructions.length < originalLength) {
            changed = true;
        }
    }
    return changed;
}
function sparseConditionalConstantPropagation(func, cfg) {
    function initState() {
        return {
            values: /* @__PURE__ */ new Map(),
            executableEdges: /* @__PURE__ */ new Set(),
            ssaWorkList: [],
            cfgWorkList: []
        };
    }
    function getValue(vreg, state2) {
        if (!state2.values.has(vreg.id)) {
            state2.values.set(vreg.id, { kind: "top" });
        }
        return state2.values.get(vreg.id);
    }
    function setValue(vreg, value, state2) {
        const oldValue = getValue(vreg, state2);
        if (oldValue.kind === value.kind) {
            if (value.kind === "constant" && oldValue.kind === "constant") {
                return oldValue.value !== value.value;
            }
            return false;
        }
        if (oldValue.kind === "bottom") return false;
        if (oldValue.kind === "constant" && value.kind === "top") return false;
        state2.values.set(vreg.id, value);
        return true;
    }
    function markEdgeExecutable(from, to, state2) {
        const edge = `${from}->${to}`;
        if (state2.executableEdges.has(edge)) {
            return false;
        }
        state2.executableEdges.add(edge);
        state2.cfgWorkList.push(to);
        return true;
    }
    function isBlockExecutable(label, state2) {
        if (label === func.entryBlock) return true;
        const preds = cfg.predecessors.get(label) || /* @__PURE__ */ new Set();
        for (const pred of preds) {
            if (state2.executableEdges.has(`${pred}->${label}`)) {
                return true;
            }
        }
        return false;
    }
    function meet(v1, v2) {
        if (v1.kind === "top") return v2;
        if (v2.kind === "top") return v1;
        if (v1.kind === "bottom" || v2.kind === "bottom") return { kind: "bottom" };
        if (v1.kind === "constant" && v2.kind === "constant") {
            if (v1.value === v2.value) {
                return v1;
            }
            return { kind: "bottom" };
        }
        return { kind: "bottom" };
    }
    function evaluateOperation(op, lhs, rhs) {
        if (lhs.kind === "top" || rhs.kind === "top") {
            return { kind: "top" };
        }
        if (lhs.kind === "bottom" || rhs.kind === "bottom") {
            return { kind: "bottom" };
        }
        const l = lhs.value;
        const r = rhs.value;
        let result;
        switch (op) {
            case "add":
                result = l + r;
                break;
            case "sub":
                result = l - r;
                break;
            case "mul":
                result = l * r;
                break;
            case "div":
                if (r === 0) return { kind: "bottom" };
                result = Math.floor(l / r);
                break;
            case "mod":
                if (r === 0) return { kind: "bottom" };
                result = l % r;
                break;
            default:
                return { kind: "bottom" };
        }
        return { kind: "constant", value: result };
    }
    function evaluateComparison(cond, lhs, rhs) {
        if (lhs.kind === "top" || rhs.kind === "top") {
            return { kind: "top" };
        }
        if (lhs.kind === "bottom" || rhs.kind === "bottom") {
            return { kind: "bottom" };
        }
        const l = lhs.value;
        const r = rhs.value;
        let result;
        switch (cond) {
            case "eq":
                result = l === r;
                break;
            case "ne":
                result = l !== r;
                break;
            case "lt":
                result = l < r;
                break;
            case "le":
                result = l <= r;
                break;
            case "gt":
                result = l > r;
                break;
            case "ge":
                result = l >= r;
                break;
            default:
                return { kind: "bottom" };
        }
        return { kind: "constant", value: result ? 1 : 0 };
    }
    function visitPhi(inst, state2) {
        if (inst.op !== "phi") return;
        const currentBlock = Array.from(func.blocks.entries()).find(
            ([_, block]) => block.instructions.includes(inst)
        )?.[0];
        if (!currentBlock) return;
        let result = { kind: "top" };
        for (const source of inst.sources) {
            if (state2.executableEdges.has(`${source.pred}->${currentBlock}`)) {
                const sourceValue = getValue(source.vreg, state2);
                result = meet(result, sourceValue);
            }
        }
        if (setValue(inst.dst, result, state2)) {
            state2.ssaWorkList.push(inst.dst);
        }
    }
    function visitInstruction(inst, state2) {
        if (inst.op === "phi") {
            visitPhi(inst, state2);
            return;
        }
        let newValue;
        switch (inst.op) {
            case "loadi":
                newValue = { kind: "constant", value: inst.imm };
                break;
            case "mov":
                newValue = getValue(inst.src, state2);
                break;
            case "add":
            case "sub":
            case "mul":
            case "div":
            case "mod":
                const lhs = getValue(inst.lhs, state2);
                const rhs = getValue(inst.rhs, state2);
                newValue = evaluateOperation(inst.op, lhs, rhs);
                break;
            case "cmp":
                const cmpLhs = getValue(inst.lhs, state2);
                const cmpRhs = getValue(inst.rhs, state2);
                newValue = evaluateComparison(inst.cond, cmpLhs, cmpRhs);
                break;
            case "alloca":
            case "load":
            case "store":
            case "call":
                newValue = { kind: "bottom" };
                break;
            default:
                newValue = { kind: "bottom" };
        }
        const dst = instGetDef(inst);
        if (dst && setValue(dst, newValue, state2)) {
            state2.ssaWorkList.push(dst);
        }
    }
    function visitTerminator(term, currentBlock, state2, block) {
        switch (term.op) {
            case "jump":
                markEdgeExecutable(currentBlock, term.target, state2);
                break;
            case "branch":
                const condValue = getValue(term.cond, state2);
                if (condValue.kind === "constant") {
                    const target = condValue.value !== 0 ? term.trueTarget : term.falseTarget;
                    markEdgeExecutable(currentBlock, target, state2);
                } else if (condValue.kind === "bottom") {
                    markEdgeExecutable(currentBlock, term.trueTarget, state2);
                    markEdgeExecutable(currentBlock, term.falseTarget, state2);
                } else {
                    markEdgeExecutable(currentBlock, term.trueTarget, state2);
                    markEdgeExecutable(currentBlock, term.falseTarget, state2);
                }
                break;
            case "ret":
                break;
        }
    }
    const state = initState();
    for (const param of func.params) {
        setValue(param, { kind: "bottom" }, state);
    }
    state.cfgWorkList.push(func.entryBlock);
    let iterations = 0;
    const MAX_ITERATIONS = 100;
    while ((state.cfgWorkList.length > 0 || state.ssaWorkList.length > 0) && iterations < MAX_ITERATIONS) {
        iterations++;
        while (state.cfgWorkList.length > 0) {
            const blockLabel = state.cfgWorkList.pop();
            const block = func.blocks.get(blockLabel);
            if (!block) continue;
            for (const inst of block.instructions) {
                if (inst.op === "phi") {
                    visitPhi(inst, state);
                } else {
                    visitInstruction(inst, state);
                }
            }
            visitTerminator(block.terminator, blockLabel, state, block);
        }
        while (state.ssaWorkList.length > 0) {
            const vreg = state.ssaWorkList.pop();
            for (const [blockLabel, block] of func.blocks) {
                if (!isBlockExecutable(blockLabel, state)) continue;
                for (const inst of block.instructions) {
                    const uses = instGetUses(inst);
                    if (uses.some((u) => vregEqual(u, vreg))) {
                        visitInstruction(inst, state);
                    }
                }
                const termUses = termGetUses(block.terminator);
                if (termUses.some((u) => vregEqual(u, vreg))) {
                    visitTerminator(block.terminator, blockLabel, state, block);
                }
            }
        }
    }
    let changed = false;
    let constantsFound = 0;
    let deadBlocksFound = 0;
    for (const [vregId, value] of state.values) {
        if (value.kind === "constant") {
            for (const block of func.blocks.values()) {
                for (let i = 0; i < block.instructions.length; i++) {
                    const inst = block.instructions[i];
                    const def = instGetDef(inst);
                    if (def && def.id === vregId) {
                        block.instructions[i] = {
                            op: "loadi",
                            dst: def,
                            imm: value.value
                        };
                        constantsFound++;
                        changed = true;
                    }
                }
            }
        }
    }
    return changed;
}
function eliminateIdentityMoves(func) {
    let changed = false;
    for (const block of func.blocks.values()) {
        block.instructions = block.instructions.filter((inst) => {
            if (inst.op === "mov") {
                const x = vregEqual(inst.dst, inst.src);
                const y = inst.dst.kind === "var" && inst.src.kind === "var";
                if (!y) {
                    return true;
                }
                const z = inst.dst.sourceName === inst.src.sourceName;
                if (!z) {
                    return true;
                }
                changed = true;
                return false;
            }
            return true;
        });
    }
    return changed;
}
function updateCFG(func, cfg) {
    for (const label of cfg.predecessors.keys()) {
        if (!func.blocks.has(label)) {
            cfg.predecessors.delete(label);
            cfg.successors.delete(label);
        }
    }
    for (const [label, preds] of cfg.predecessors) {
        for (const pred of Array.from(preds)) {
            if (!func.blocks.has(pred)) {
                preds.delete(pred);
            }
        }
    }
    for (const [label, succs] of cfg.successors) {
        for (const succ of Array.from(succs)) {
            if (!func.blocks.has(succ)) {
                succs.delete(succ);
            }
        }
    }
}
function postSSA(ir) {
    for (const func of ir.functions) {
        eliminateIdentityMoves(func);
        mergeBranches(func);
        removeDeadBlocks(func, ir.cfg.functionCFGs.get(func.name));
    }
}
function removeDeadBlocks(func, cfg) {
    let changed = true;
    while (changed) {
        changed = false;
        rebuildCFG(func, cfg);
        const trampolinesToRemove = [];
        for (const [key, block] of func.blocks) {
            if (block.instructions.length === 0 && block.terminator.op === "jump") {
                trampolinesToRemove.push(key);
            }
        }
        for (const key of trampolinesToRemove) {
            const block = func.blocks.get(key);
            const target = block.terminator.target;
            for (const [key2, block2] of func.blocks) {
                if (key2 === key || block2.terminator.op === "ret") continue;
                if (block2.terminator.op === "jump" && block2.terminator.target === key) {
                    block2.terminator.target = target;
                    changed = true;
                }
                if (block2.terminator.op === "branch") {
                    if (block2.terminator.trueTarget === key) {
                        block2.terminator.trueTarget = target;
                        changed = true;
                    }
                    if (block2.terminator.falseTarget === key) {
                        block2.terminator.falseTarget = target;
                        changed = true;
                    }
                }
            }
            if (func.entryBlock === key) {
                func.entryBlock = target;
            }
            func.blocks.delete(key);
            changed = true;
        }
        if (changed) continue;
        for (const [label, block] of func.blocks) {
            if (block.terminator.op === "jump") {
                const target = block.terminator.target;
                const targetBlock = func.blocks.get(target);
                if (!targetBlock) continue;
                const preds = cfg.predecessors.get(target);
                if (preds?.size === 1 && preds.has(label)) {
                    block.instructions.push(...targetBlock.instructions);
                    block.terminator = targetBlock.terminator;
                    func.blocks.delete(target);
                    changed = true;
                    break;
                }
            }
        }
    }
    const reachable = /* @__PURE__ */ new Set([func.entryBlock]);
    const worklist = [func.entryBlock];
    while (worklist.length > 0) {
        const label = worklist.pop();
        const block = func.blocks.get(label);
        if (!block) continue;
        if (block.terminator.op === "jump") {
            if (!reachable.has(block.terminator.target)) {
                reachable.add(block.terminator.target);
                worklist.push(block.terminator.target);
            }
        } else if (block.terminator.op === "branch") {
            if (!reachable.has(block.terminator.trueTarget)) {
                reachable.add(block.terminator.trueTarget);
                worklist.push(block.terminator.trueTarget);
            }
            if (!reachable.has(block.terminator.falseTarget)) {
                reachable.add(block.terminator.falseTarget);
                worklist.push(block.terminator.falseTarget);
            }
        }
    }
    for (const label of Array.from(func.blocks.keys())) {
        if (!reachable.has(label)) {
            func.blocks.delete(label);
        }
    }
    rebuildCFG(func, cfg);
}
function rebuildCFG(func, cfg) {
    cfg.predecessors.clear();
    cfg.successors.clear();
    for (const [label, block] of func.blocks) {
        if (!cfg.predecessors.has(label)) cfg.predecessors.set(label, /* @__PURE__ */ new Set());
        if (!cfg.successors.has(label)) cfg.successors.set(label, /* @__PURE__ */ new Set());
        if (block.terminator.op === "jump") {
            const target = block.terminator.target;
            if (!cfg.predecessors.has(target))
                cfg.predecessors.set(target, /* @__PURE__ */ new Set());
            cfg.successors.get(label).add(target);
            cfg.predecessors.get(target).add(label);
        } else if (block.terminator.op === "branch") {
            const t1 = block.terminator.trueTarget;
            const t2 = block.terminator.falseTarget;
            if (!cfg.predecessors.has(t1)) cfg.predecessors.set(t1, /* @__PURE__ */ new Set());
            if (!cfg.predecessors.has(t2)) cfg.predecessors.set(t2, /* @__PURE__ */ new Set());
            cfg.successors.get(label).add(t1);
            cfg.successors.get(label).add(t2);
            cfg.predecessors.get(t1).add(label);
            cfg.predecessors.get(t2).add(label);
        }
    }
}

// src/parser.ts
function isAlpha(c) {
    return c >= "a" && c <= "z" || c >= "A" && c <= "Z";
}
function isDigit(c) {
    return c >= "0" && c <= "9";
}
function isIdentStart(c) {
    return isAlpha(c) || c === "_";
}
function isIdentContinue(c) {
    return isAlpha(c) || isDigit(c) || c === "_";
}
function peek(p, offset = 0) {
    const idx = p.current + offset;
    if (idx >= p.source.length) {
        return "\0";
    }
    return p.source[idx];
}
function advance(p) {
    if (p.current >= p.source.length) {
        return "\0";
    }
    const c = p.source[p.current++];
    if (c === "\n") {
        p.line++;
        p.lineStart = p.current;
    }
    return c;
}
function skipWhitespace(p) {
    while (true) {
        const c = peek(p);
        if (c === " " || c === "	" || c === "\r" || c === "\n") {
            advance(p);
        } else if (c === "/" && peek(p, 1) === "/") {
            advance(p);
            advance(p);
            while (peek(p) !== "\n" && peek(p) !== "\0") {
                advance(p);
            }
        } else {
            return;
        }
    }
}
function setError(p, message) {
    if (!p.error) {
        p.error = {
            message,
            line: p.line,
            column: p.current - p.lineStart + 1
        };
    }
}
function expectToken(p, kind) {
    const tok = nextToken(p);
    if (tok.kind !== kind) {
        setError(
            p,
            `Expected ${tokenKindToString(kind)} but got ${tokenKindToString(tok.kind)}`
        );
        return false;
    }
    return true;
}
function tokenKindToString(kind) {
    switch (kind) {
        case 0 /* TOK_INT */:
            return "'int'";
        case 1 /* TOK_VOID */:
            return "'void'";
        case 2 /* TOK_IF */:
            return "'if'";
        case 3 /* TOK_ELSE */:
            return "'else'";
        case 4 /* TOK_WHILE */:
            return "'while'";
        case 5 /* TOK_FOR */:
            return "'for'";
        case 6 /* TOK_RETURN */:
            return "'return'";
        case 9 /* TOK_GOTO */:
            return "'goto'";
        case 12 /* TOK_IDENTIFIER */:
            return "identifier";
        case 11 /* TOK_INT_LITERAL */:
            return "number";
        case 35 /* TOK_SEMICOLON */:
            return "';'";
        case 36 /* TOK_COMMA */:
            return "','";
        case 29 /* TOK_LPAREN */:
            return "'('";
        case 30 /* TOK_RPAREN */:
            return "')'";
        case 31 /* TOK_LBRACE */:
            return "'{'";
        case 32 /* TOK_RBRACE */:
            return "'}'";
        case 33 /* TOK_LBRACKET */:
            return "'['";
        case 34 /* TOK_RBRACKET */:
            return "']'";
        case 37 /* TOK_EOF */:
            return "end of file";
        default:
            return "token";
    }
}
function peekToken(p) {
    const saved = { ...p };
    const tok = nextToken(p);
    p.current = saved.current;
    p.line = saved.line;
    p.lineStart = saved.lineStart;
    return tok;
}
function nextToken(p) {
    skipWhitespace(p);
    const line = p.line;
    const column = p.current - p.lineStart + 1;
    const start = p.current;
    const c = peek(p);
    if (c === "\0") {
        return { kind: 37 /* TOK_EOF */, text: "", intValue: 0, line, column };
    }
    switch (c) {
        case "(":
            advance(p);
            return {
                kind: 29 /* TOK_LPAREN */,
                text: "(",
                intValue: 0,
                line,
                column
            };
        case ")":
            advance(p);
            return {
                kind: 30 /* TOK_RPAREN */,
                text: ")",
                intValue: 0,
                line,
                column
            };
        case "{":
            advance(p);
            return {
                kind: 31 /* TOK_LBRACE */,
                text: "{",
                intValue: 0,
                line,
                column
            };
        case "}":
            advance(p);
            return {
                kind: 32 /* TOK_RBRACE */,
                text: "}",
                intValue: 0,
                line,
                column
            };
        case "[":
            advance(p);
            return {
                kind: 33 /* TOK_LBRACKET */,
                text: "[",
                intValue: 0,
                line,
                column
            };
        case "]":
            advance(p);
            return {
                kind: 34 /* TOK_RBRACKET */,
                text: "]",
                intValue: 0,
                line,
                column
            };
        case ";":
            advance(p);
            return {
                kind: 35 /* TOK_SEMICOLON */,
                text: ";",
                intValue: 0,
                line,
                column
            };
        case ",":
            advance(p);
            return {
                kind: 36 /* TOK_COMMA */,
                text: ",",
                intValue: 0,
                line,
                column
            };
        case ":":
            advance(p);
            return {
                kind: 10 /* TOK_COLON */,
                text: ":",
                intValue: 0,
                line,
                column
            };
        case "*":
            advance(p);
            return { kind: 15 /* TOK_STAR */, text: "*", intValue: 0, line, column };
        case "/":
            advance(p);
            return {
                kind: 16 /* TOK_SLASH */,
                text: "/",
                intValue: 0,
                line,
                column
            };
        case "%":
            advance(p);
            return {
                kind: 17 /* TOK_PERCENT */,
                text: "%",
                intValue: 0,
                line,
                column
            };
        case "+":
            advance(p);
            if (peek(p) === "+") {
                advance(p);
                return {
                    kind: 7 /* TOK_INC */,
                    text: "++",
                    intValue: 0,
                    line,
                    column
                };
            }
            return { kind: 13 /* TOK_PLUS */, text: "+", intValue: 0, line, column };
        case "-":
            advance(p);
            if (peek(p) === "-") {
                advance(p);
                return {
                    kind: 8 /* TOK_DEC */,
                    text: "--",
                    intValue: 0,
                    line,
                    column
                };
            }
            return {
                kind: 14 /* TOK_MINUS */,
                text: "-",
                intValue: 0,
                line,
                column
            };
        case "<":
            advance(p);
            if (peek(p) === "=") {
                advance(p);
                return {
                    kind: 20 /* TOK_LTE */,
                    text: "<=",
                    intValue: 0,
                    line,
                    column
                };
            }
            return { kind: 18 /* TOK_LT */, text: "<", intValue: 0, line, column };
        case ">":
            advance(p);
            if (peek(p) === "=") {
                advance(p);
                return {
                    kind: 21 /* TOK_GTE */,
                    text: ">=",
                    intValue: 0,
                    line,
                    column
                };
            }
            return { kind: 19 /* TOK_GT */, text: ">", intValue: 0, line, column };
        case "=":
            advance(p);
            if (peek(p) === "=") {
                advance(p);
                return {
                    kind: 22 /* TOK_EQ */,
                    text: "==",
                    intValue: 0,
                    line,
                    column
                };
            }
            return {
                kind: 27 /* TOK_ASSIGN */,
                text: "=",
                intValue: 0,
                line,
                column
            };
        case "!":
            advance(p);
            if (peek(p) === "=") {
                advance(p);
                return {
                    kind: 23 /* TOK_NEQ */,
                    text: "!=",
                    intValue: 0,
                    line,
                    column
                };
            }
            return { kind: 26 /* TOK_NOT */, text: "!", intValue: 0, line, column };
        case "&":
            advance(p);
            if (peek(p) === "&") {
                advance(p);
                return {
                    kind: 24 /* TOK_AND */,
                    text: "&&",
                    intValue: 0,
                    line,
                    column
                };
            }
            return {
                kind: 28 /* TOK_AMPERSAND */,
                text: "&",
                intValue: 0,
                line,
                column
            };
        case "|":
            advance(p);
            if (peek(p) === "|") {
                advance(p);
                return {
                    kind: 25 /* TOK_OR */,
                    text: "||",
                    intValue: 0,
                    line,
                    column
                };
            }
            setError(p, "Unexpected character '|'");
            return { kind: 37 /* TOK_EOF */, text: "", intValue: 0, line, column };
    }
    if (isIdentStart(c)) {
        advance(p);
        while (isIdentContinue(peek(p))) {
            advance(p);
        }
        const text = p.source.substring(start, p.current);
        let kind = 12 /* TOK_IDENTIFIER */;
        switch (text) {
            case "int":
                kind = 0 /* TOK_INT */;
                break;
            case "void":
                kind = 1 /* TOK_VOID */;
                break;
            case "if":
                kind = 2 /* TOK_IF */;
                break;
            case "else":
                kind = 3 /* TOK_ELSE */;
                break;
            case "while":
                kind = 4 /* TOK_WHILE */;
                break;
            case "for":
                kind = 5 /* TOK_FOR */;
                break;
            case "return":
                kind = 6 /* TOK_RETURN */;
                break;
            case "goto":
                kind = 9 /* TOK_GOTO */;
                break;
        }
        return { kind, text, intValue: 0, line, column };
    }
    if (isDigit(c)) {
        let value = 0;
        while (isDigit(peek(p))) {
            const digit = advance(p).charCodeAt(0) - "0".charCodeAt(0);
            if (value > (2147483647 - digit) / 10) {
                setError(p, "Integer literal too large");
                return { kind: 37 /* TOK_EOF */, text: "", intValue: 0, line, column };
            }
            value = value * 10 + digit;
        }
        return {
            kind: 11 /* TOK_INT_LITERAL */,
            text: p.source.substring(start, p.current),
            intValue: value,
            line,
            column
        };
    }
    setError(p, `Unexpected character '${c}'`);
    advance(p);
    return { kind: 37 /* TOK_EOF */, text: "", intValue: 0, line, column };
}
function parseType(p) {
    const tok = peekToken(p);
    let baseType;
    if (tok.kind === 1 /* TOK_VOID */) {
        nextToken(p);
        baseType = { kind: "void" };
    } else if (tok.kind === 0 /* TOK_INT */) {
        nextToken(p);
        baseType = { kind: "int" };
    } else {
        setError(p, "Expected type");
        return null;
    }
    while (peekToken(p).kind === 15 /* TOK_STAR */) {
        nextToken(p);
        baseType = { kind: "pointer", base: baseType };
    }
    return baseType;
}
function parsePrimary(p) {
    const tok = nextToken(p);
    if (tok.kind === 11 /* TOK_INT_LITERAL */) {
        return { kind: "int_literal", value: tok.intValue };
    }
    if (tok.kind === 12 /* TOK_IDENTIFIER */) {
        return { kind: "identifier", name: tok.text };
    }
    if (tok.kind === 29 /* TOK_LPAREN */) {
        const expr = parseExpr(p);
        if (!expr) return null;
        if (!expectToken(p, 30 /* TOK_RPAREN */)) return null;
        return expr;
    }
    setError(p, "Expected expression");
    return null;
}
function parsePostfix(p) {
    let expr = parsePrimary(p);
    if (!expr || p.error) return null;
    while (!p.error) {
        const tok = peekToken(p);
        if (tok.kind === 29 /* TOK_LPAREN */) {
            nextToken(p);
            const args = [];
            while (peekToken(p).kind !== 30 /* TOK_RPAREN */ && peekToken(p).kind !== 37 /* TOK_EOF */) {
                const arg = parseAssignment(p);
                if (!arg || p.error) return null;
                args.push(arg);
                if (peekToken(p).kind === 36 /* TOK_COMMA */) {
                    nextToken(p);
                } else {
                    break;
                }
            }
            if (!expectToken(p, 30 /* TOK_RPAREN */)) return null;
            expr = { kind: "call", func: expr, args };
        } else if (tok.kind === 33 /* TOK_LBRACKET */) {
            nextToken(p);
            const index = parseExpr(p);
            if (!index || p.error) return null;
            if (!expectToken(p, 34 /* TOK_RBRACKET */)) return null;
            expr = { kind: "index", array: expr, index };
        } else if (tok.kind === 7 /* TOK_INC */) {
            nextToken(p);
            expr = { kind: "unary", op: "post_inc", operand: expr };
        } else if (tok.kind === 8 /* TOK_DEC */) {
            nextToken(p);
            expr = { kind: "unary", op: "post_dec", operand: expr };
        } else {
            break;
        }
    }
    return expr;
}
function parseUnary(p) {
    const tok = peekToken(p);
    if (tok.kind === 7 /* TOK_INC */) {
        nextToken(p);
        const operand = parseUnary(p);
        if (!operand || p.error) return null;
        return { kind: "unary", op: "pre_inc", operand };
    }
    if (tok.kind === 8 /* TOK_DEC */) {
        nextToken(p);
        const operand = parseUnary(p);
        if (!operand || p.error) return null;
        return { kind: "unary", op: "pre_dec", operand };
    }
    if (tok.kind === 14 /* TOK_MINUS */) {
        nextToken(p);
        const next = peekToken(p);
        if (next.kind === 11 /* TOK_INT_LITERAL */) {
            nextToken(p);
            return { kind: "int_literal", value: -next.intValue };
        }
        const operand = parseUnary(p);
        if (!operand || p.error) return null;
        return { kind: "unary", op: "negate", operand };
    }
    if (tok.kind === 26 /* TOK_NOT */) {
        nextToken(p);
        const operand = parseUnary(p);
        if (!operand || p.error) return null;
        return { kind: "unary", op: "not", operand };
    }
    if (tok.kind === 28 /* TOK_AMPERSAND */) {
        nextToken(p);
        const operand = parseUnary(p);
        if (!operand || p.error) return null;
        return { kind: "unary", op: "address", operand };
    }
    if (tok.kind === 15 /* TOK_STAR */) {
        nextToken(p);
        const operand = parseUnary(p);
        if (!operand || p.error) return null;
        return { kind: "unary", op: "deref", operand };
    }
    return parsePostfix(p);
}
function parseBinaryExpr(p, parseOperand, ops) {
    let left = parseOperand(p);
    if (!left || p.error) return null;
    while (!p.error) {
        const tok = peekToken(p);
        if (!ops.includes(tok.kind)) break;
        nextToken(p);
        const op = tokenToBinaryOp(tok.kind);
        const right = parseOperand(p);
        if (!right || p.error) return null;
        left = { kind: "binary", op, left, right };
    }
    return left;
}
function tokenToBinaryOp(kind) {
    switch (kind) {
        case 13 /* TOK_PLUS */:
            return "add";
        case 14 /* TOK_MINUS */:
            return "sub";
        case 15 /* TOK_STAR */:
            return "mul";
        case 16 /* TOK_SLASH */:
            return "div";
        case 17 /* TOK_PERCENT */:
            return "mod";
        case 18 /* TOK_LT */:
            return "lt";
        case 19 /* TOK_GT */:
            return "gt";
        case 20 /* TOK_LTE */:
            return "lte";
        case 21 /* TOK_GTE */:
            return "gte";
        case 22 /* TOK_EQ */:
            return "eq";
        case 23 /* TOK_NEQ */:
            return "neq";
        case 24 /* TOK_AND */:
            return "and";
        case 25 /* TOK_OR */:
            return "or";
        default:
            return "add";
    }
}
function parseMultiplicative(p) {
    return parseBinaryExpr(p, parseUnary, [
        15 /* TOK_STAR */,
        16 /* TOK_SLASH */,
        17 /* TOK_PERCENT */
    ]);
}
function parseAdditive(p) {
    return parseBinaryExpr(p, parseMultiplicative, [
        13 /* TOK_PLUS */,
        14 /* TOK_MINUS */
    ]);
}
function parseRelational(p) {
    return parseBinaryExpr(p, parseAdditive, [
        18 /* TOK_LT */,
        19 /* TOK_GT */,
        20 /* TOK_LTE */,
        21 /* TOK_GTE */
    ]);
}
function parseEquality(p) {
    return parseBinaryExpr(p, parseRelational, [
        22 /* TOK_EQ */,
        23 /* TOK_NEQ */
    ]);
}
function parseLogicalAnd(p) {
    return parseBinaryExpr(p, parseEquality, [24 /* TOK_AND */]);
}
function parseLogicalOr(p) {
    return parseBinaryExpr(p, parseLogicalAnd, [25 /* TOK_OR */]);
}
function parseAssignment(p) {
    const left = parseLogicalOr(p);
    if (!left || p.error) return null;
    if (peekToken(p).kind === 27 /* TOK_ASSIGN */) {
        nextToken(p);
        const right = parseAssignment(p);
        if (!right || p.error) return null;
        return { kind: "assign", target: left, value: right };
    }
    return left;
}
function parseExpr(p) {
    return parseAssignment(p);
}
function parseStmt(p) {
    let label = "";
    const tok1 = peekToken(p);
    if (tok1.kind === 12 /* TOK_IDENTIFIER */) {
        const saved = { ...p };
        nextToken(p);
        if (peekToken(p).kind === 10 /* TOK_COLON */) {
            nextToken(p);
            label = tok1.text;
            if (peekToken(p).kind === 32 /* TOK_RBRACE */ || peekToken(p).kind === 37 /* TOK_EOF */) {
                return { kind: "empty", label };
            }
        } else {
            p.current = saved.current;
            p.line = saved.line;
            p.lineStart = saved.lineStart;
        }
    }
    const tok = peekToken(p);
    if (tok.kind === 31 /* TOK_LBRACE */) {
        nextToken(p);
        const stmts = [];
        while (peekToken(p).kind !== 32 /* TOK_RBRACE */ && peekToken(p).kind !== 37 /* TOK_EOF */ && !p.error) {
            const stmt = parseStmt(p);
            if (!stmt || p.error) return null;
            stmts.push(stmt);
        }
        if (!expectToken(p, 32 /* TOK_RBRACE */)) return null;
        return { kind: "block", label, stmts };
    }
    if (tok.kind === 2 /* TOK_IF */) {
        nextToken(p);
        if (!expectToken(p, 29 /* TOK_LPAREN */)) return null;
        const cond = parseExpr(p);
        if (!cond || p.error) return null;
        if (!expectToken(p, 30 /* TOK_RPAREN */)) return null;
        const thenStmt = parseStmt(p);
        if (!thenStmt || p.error) return null;
        let elseStmt = null;
        if (peekToken(p).kind === 3 /* TOK_ELSE */) {
            nextToken(p);
            elseStmt = parseStmt(p);
            if (!elseStmt || p.error) return null;
        }
        return { kind: "if", label, cond, thenStmt, elseStmt };
    }
    if (tok.kind === 4 /* TOK_WHILE */) {
        nextToken(p);
        if (!expectToken(p, 29 /* TOK_LPAREN */)) return null;
        const cond = parseExpr(p);
        if (!cond || p.error) return null;
        if (!expectToken(p, 30 /* TOK_RPAREN */)) return null;
        const body = parseStmt(p);
        if (!body || p.error) return null;
        return { kind: "while", label, cond, body };
    }
    if (tok.kind === 5 /* TOK_FOR */) {
        nextToken(p);
        if (!expectToken(p, 29 /* TOK_LPAREN */)) return null;
        let init2 = null;
        if (peekToken(p).kind === 0 /* TOK_INT */) {
            const decl = parseDecl(p);
            if (!decl || p.error) return null;
            init2 = { kind: "decl", label: "", decl };
        } else if (peekToken(p).kind !== 35 /* TOK_SEMICOLON */) {
            const expr2 = parseExpr(p);
            if (expr2 && !p.error) {
                init2 = { kind: "expr", label: "", expr: expr2 };
            }
        }
        if (!expectToken(p, 35 /* TOK_SEMICOLON */)) return null;
        let cond = null;
        if (peekToken(p).kind !== 35 /* TOK_SEMICOLON */) {
            cond = parseExpr(p);
            if (p.error) return null;
        }
        if (!expectToken(p, 35 /* TOK_SEMICOLON */)) return null;
        let update = null;
        if (peekToken(p).kind !== 30 /* TOK_RPAREN */) {
            update = parseExpr(p);
            if (p.error) return null;
        }
        if (!expectToken(p, 30 /* TOK_RPAREN */)) return null;
        const body = parseStmt(p);
        if (!body || p.error) return null;
        return { kind: "for", label, init: init2, cond, update, body };
    }
    if (tok.kind === 6 /* TOK_RETURN */) {
        nextToken(p);
        let value = null;
        if (peekToken(p).kind !== 35 /* TOK_SEMICOLON */) {
            value = parseExpr(p);
            if (p.error) return null;
        }
        if (!expectToken(p, 35 /* TOK_SEMICOLON */)) return null;
        return { kind: "return", label, value };
    }
    if (tok.kind === 9 /* TOK_GOTO */) {
        nextToken(p);
        const target = nextToken(p);
        if (target.kind !== 12 /* TOK_IDENTIFIER */) {
            setError(p, "Expected label after 'goto'");
            return null;
        }
        if (!expectToken(p, 35 /* TOK_SEMICOLON */)) return null;
        return { kind: "goto", label, target: target.text };
    }
    if (tok.kind === 35 /* TOK_SEMICOLON */) {
        nextToken(p);
        return { kind: "empty", label };
    }
    if (tok.kind === 0 /* TOK_INT */ || tok.kind === 1 /* TOK_VOID */) {
        const decl = parseDecl(p);
        if (!decl || p.error) return null;
        if (!expectToken(p, 35 /* TOK_SEMICOLON */)) return null;
        return { kind: "decl", label, decl };
    }
    const expr = parseExpr(p);
    if (!expr || p.error) return null;
    if (!expectToken(p, 35 /* TOK_SEMICOLON */)) return null;
    return { kind: "expr", label, expr };
}
function parseDecl(p) {
    const type = parseType(p);
    if (!type || p.error) return null;
    const nameTok = nextToken(p);
    if (nameTok.kind !== 12 /* TOK_IDENTIFIER */) {
        setError(p, "Expected identifier in declaration");
        return null;
    }
    const name = nameTok.text;
    if (name.startsWith("_")) {
        console.log("hit");
        setError(
            p,
            "This parser reserves the prefix '_' for compiler generated temporaries"
        );
        return null;
    }
    let arrayType = type;
    while (peekToken(p).kind === 33 /* TOK_LBRACKET */) {
        nextToken(p);
        if (peekToken(p).kind === 11 /* TOK_INT_LITERAL */) {
            const sizeTok = nextToken(p);
            arrayType = { kind: "array", base: arrayType, size: sizeTok.intValue };
        } else if (peekToken(p).kind === 34 /* TOK_RBRACKET */) {
            arrayType = { kind: "pointer", base: arrayType };
        } else {
            setError(p, "Expected array size or ']'");
            return null;
        }
        if (!expectToken(p, 34 /* TOK_RBRACKET */)) return null;
    }
    if (peekToken(p).kind === 29 /* TOK_LPAREN */) {
        nextToken(p);
        const params = [];
        const paramTypes = [];
        while (peekToken(p).kind !== 30 /* TOK_RPAREN */ && peekToken(p).kind !== 37 /* TOK_EOF */ && !p.error) {
            const paramType = parseType(p);
            if (!paramType || p.error) return null;
            const paramNameTok = nextToken(p);
            if (paramNameTok.kind !== 12 /* TOK_IDENTIFIER */) {
                setError(p, "Expected parameter name");
                return null;
            }
            let finalType = paramType;
            if (peekToken(p).kind === 33 /* TOK_LBRACKET */) {
                nextToken(p);
                if (!expectToken(p, 34 /* TOK_RBRACKET */)) return null;
                finalType = { kind: "pointer", base: paramType };
            }
            const param = { name: paramNameTok.text, type: finalType };
            params.push(param);
            paramTypes.push(finalType);
            const paramDecl = {
                kind: "var",
                name: param.name,
                type: param.type,
                init: null
            };
            if (peekToken(p).kind === 36 /* TOK_COMMA */) {
                nextToken(p);
            } else {
                break;
            }
        }
        if (!expectToken(p, 30 /* TOK_RPAREN */)) return null;
        let body = null;
        if (peekToken(p).kind === 31 /* TOK_LBRACE */) {
            body = parseStmt(p);
            if (!body || p.error) return null;
        }
        const funcType = { kind: "function", returnType: type, paramTypes };
        const decl2 = { kind: "func", name, type: funcType, params, body };
        return decl2;
    }
    let init2 = null;
    if (peekToken(p).kind === 27 /* TOK_ASSIGN */) {
        nextToken(p);
        init2 = parseExpr(p);
        if (!init2 || p.error) return null;
    }
    const decl = { kind: "var", name, type: arrayType, init: init2 };
    return decl;
}
function parseProgram(p) {
    const declarations = [];
    while (peekToken(p).kind !== 37 /* TOK_EOF */ && !p.error) {
        const decl = parseDecl(p);
        if (!decl || p.error) return { declarations };
        declarations.push(decl);
        if (decl.kind === "var") {
            if (!expectToken(p, 35 /* TOK_SEMICOLON */)) return { declarations };
        }
    }
    return { declarations };
}
function parse(source) {
    const parser = {
        source,
        current: 0,
        line: 1,
        lineStart: 0,
        error: null
    };
    const ast = parseProgram(parser);
    if (parser.error) {
        return {
            ast,
            error: `Line ${parser.error.line}:${parser.error.column}: ${parser.error.message}`
        };
    }
    return {
        ast,
        error: ""
    };
}

// src/printers.ts
var AstPrinter = class {
    constructor() {
        this.indentLevel = 0;
        this.lastChild = [];
        this.output = "";
    }
    printIndent() {
        for (let i = 0; i < this.indentLevel; i++) {
            if (i === this.indentLevel - 1) {
                this.output += this.lastChild[i] ? "\u2514\u2500\u2500 " : "\u251C\u2500\u2500 ";
            } else {
                this.output += this.lastChild[i] ? "    " : "\u2502   ";
            }
        }
    }
    printType(type) {
        switch (type.kind) {
            case "void":
                return "void";
            case "int":
                return "int";
            case "pointer":
                return "*" + this.printType(type.base);
            case "array":
                return `[${type.size}]` + this.printType(type.base);
            case "function": {
                const params = type.paramTypes.map((t) => this.printType(t)).join(", ");
                return `func(${params}) -> ${this.printType(type.returnType)}`;
            }
        }
        return "";
    }
    printExpr(expr) {
        this.printIndent();
        switch (expr.kind) {
            case "int_literal":
                this.output += `INT_LITERAL: ${expr.value}
`;
                break;
            case "identifier":
                this.output += `IDENTIFIER: ${expr.name}`;
                if (expr.symbol) {
                    this.output += " [sym]";
                }
                this.output += "\n";
                break;
            case "binary":
                this.output += expr.op.toUpperCase() + "\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = false;
                this.printExpr(expr.left);
                this.lastChild[this.indentLevel - 1] = true;
                this.printExpr(expr.right);
                this.indentLevel--;
                break;
            case "unary":
                this.output += expr.op.toUpperCase().replace("_", "") + "\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printExpr(expr.operand);
                this.indentLevel--;
                break;
            case "assign":
                this.output += "ASSIGN\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = false;
                this.printExpr(expr.target);
                this.lastChild[this.indentLevel - 1] = true;
                this.printExpr(expr.value);
                this.indentLevel--;
                break;
            case "call":
                this.output += "CALL\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = false;
                this.printIndent();
                this.output += "func:\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printExpr(expr.func);
                this.indentLevel--;
                if (expr.args.length > 0) {
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printIndent();
                    this.output += "args:\n";
                    this.indentLevel++;
                    for (let i = 0; i < expr.args.length; i++) {
                        this.lastChild[this.indentLevel - 1] = i === expr.args.length - 1;
                        this.printExpr(expr.args[i]);
                    }
                    this.indentLevel--;
                }
                this.indentLevel--;
                break;
            case "index":
                this.output += "INDEX\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = false;
                this.printExpr(expr.array);
                this.lastChild[this.indentLevel - 1] = true;
                this.printExpr(expr.index);
                this.indentLevel--;
                break;
        }
    }
    printStmt(stmt) {
        this.printIndent();
        if (stmt.label && stmt.label !== "") {
            this.output += `[label: ${stmt.label}] `;
        }
        switch (stmt.kind) {
            case "expr":
                this.output += "EXPR_STMT\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printExpr(stmt.expr);
                this.indentLevel--;
                break;
            case "return":
                this.output += "RETURN\n";
                if (stmt.value) {
                    this.indentLevel++;
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printExpr(stmt.value);
                    this.indentLevel--;
                }
                break;
            case "block":
                this.output += "BLOCK\n";
                this.indentLevel++;
                for (let i = 0; i < stmt.stmts.length; i++) {
                    this.lastChild[this.indentLevel - 1] = i === stmt.stmts.length - 1;
                    this.printStmt(stmt.stmts[i]);
                }
                this.indentLevel--;
                break;
            case "if":
                this.output += "IF\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = false;
                this.printIndent();
                this.output += "cond:\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printExpr(stmt.cond);
                this.indentLevel--;
                this.lastChild[this.indentLevel - 1] = stmt.elseStmt === null;
                this.printIndent();
                this.output += "then:\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printStmt(stmt.thenStmt);
                this.indentLevel--;
                if (stmt.elseStmt) {
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printIndent();
                    this.output += "else:\n";
                    this.indentLevel++;
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printStmt(stmt.elseStmt);
                    this.indentLevel--;
                }
                this.indentLevel--;
                break;
            case "while":
                this.output += "WHILE\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = false;
                this.printIndent();
                this.output += "cond:\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printExpr(stmt.cond);
                this.indentLevel--;
                this.lastChild[this.indentLevel - 1] = true;
                this.printIndent();
                this.output += "body:\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printStmt(stmt.body);
                this.indentLevel--;
                this.indentLevel--;
                break;
            case "for":
                this.output += "FOR\n";
                this.indentLevel++;
                if (stmt.init) {
                    this.lastChild[this.indentLevel - 1] = false;
                    this.printIndent();
                    this.output += "init:\n";
                    this.indentLevel++;
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printStmt(stmt.init);
                    this.indentLevel--;
                }
                if (stmt.cond) {
                    this.lastChild[this.indentLevel - 1] = false;
                    this.printIndent();
                    this.output += "cond:\n";
                    this.indentLevel++;
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printExpr(stmt.cond);
                    this.indentLevel--;
                }
                if (stmt.update) {
                    this.lastChild[this.indentLevel - 1] = false;
                    this.printIndent();
                    this.output += "update:\n";
                    this.indentLevel++;
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printExpr(stmt.update);
                    this.indentLevel--;
                }
                this.lastChild[this.indentLevel - 1] = true;
                this.printIndent();
                this.output += "body:\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printStmt(stmt.body);
                this.indentLevel--;
                this.indentLevel--;
                break;
            case "goto":
                this.output += `GOTO: ${stmt.target}
`;
                break;
            case "empty":
                this.output += "EMPTY\n";
                break;
            case "decl":
                this.output += "DECL_STMT\n";
                this.indentLevel++;
                this.lastChild[this.indentLevel - 1] = true;
                this.printDecl(stmt.decl);
                this.indentLevel--;
                break;
        }
    }
    printDecl(decl) {
        this.printIndent();
        switch (decl.kind) {
            case "var":
                this.output += `VAR_DECL: ${decl.name} ${this.printType(decl.type)}
`;
                if (decl.init) {
                    this.indentLevel++;
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printIndent();
                    this.output += "init:\n";
                    this.indentLevel++;
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printExpr(decl.init);
                    this.indentLevel--;
                    this.indentLevel--;
                }
                break;
            case "func":
                this.output += `FUNC_DECL: ${decl.name} ${this.printType(decl.type)}
`;
                this.indentLevel++;
                if (decl.params.length > 0) {
                    this.lastChild[this.indentLevel - 1] = decl.body === null;
                    this.printIndent();
                    this.output += "params:\n";
                    this.indentLevel++;
                    for (let i = 0; i < decl.params.length; i++) {
                        this.lastChild[this.indentLevel - 1] = i === decl.params.length - 1;
                        this.printIndent();
                        this.output += `PARAM: ${decl.params[i].name} ${this.printType(decl.params[i].type)}
`;
                    }
                    this.indentLevel--;
                }
                if (decl.body) {
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printIndent();
                    this.output += "body:\n";
                    this.indentLevel++;
                    this.lastChild[this.indentLevel - 1] = true;
                    this.printStmt(decl.body);
                    this.indentLevel--;
                }
                this.indentLevel--;
                break;
        }
    }
    printProgram(program) {
        this.output = "PROGRAM\n";
        this.indentLevel = 1;
        for (let i = 0; i < program.declarations.length; i++) {
            this.lastChild[0] = i === program.declarations.length - 1;
            this.printDecl(program.declarations[i]);
        }
        return this.output;
    }
};
var CEmitter = class {
    constructor() {
        this.indentLevel = 0;
        this.output = "";
    }
    indent() {
        for (let i = 0; i < this.indentLevel; i++) {
            this.output += "    ";
        }
    }
    emitType(type) {
        switch (type.kind) {
            case "void":
                this.output += "void";
                break;
            case "int":
                this.output += "int";
                break;
            case "pointer":
                this.emitType(type.base);
                this.output += "*";
                break;
            case "array":
                this.emitType(type.base);
                break;
            case "function":
                this.emitType(type.returnType);
                break;
        }
    }
    emitArrayDimensions(type) {
        if (type.kind === "array") {
            this.output += `[${type.size}]`;
            if (type.base.kind === "array") {
                this.emitArrayDimensions(type.base);
            }
        }
    }
    emitExpr(expr) {
        switch (expr.kind) {
            case "int_literal":
                this.output += expr.value;
                break;
            case "identifier":
                this.output += expr.name;
                break;
            case "binary":
                this.output += "(";
                this.emitExpr(expr.left);
                switch (expr.op) {
                    case "add":
                        this.output += " + ";
                        break;
                    case "sub":
                        this.output += " - ";
                        break;
                    case "mul":
                        this.output += " * ";
                        break;
                    case "div":
                        this.output += " / ";
                        break;
                    case "mod":
                        this.output += " % ";
                        break;
                    case "lt":
                        this.output += " < ";
                        break;
                    case "gt":
                        this.output += " > ";
                        break;
                    case "lte":
                        this.output += " <= ";
                        break;
                    case "gte":
                        this.output += " >= ";
                        break;
                    case "eq":
                        this.output += " == ";
                        break;
                    case "neq":
                        this.output += " != ";
                        break;
                    case "and":
                        this.output += " && ";
                        break;
                    case "or":
                        this.output += " || ";
                        break;
                }
                this.emitExpr(expr.right);
                this.output += ")";
                break;
            case "unary":
                switch (expr.op) {
                    case "negate":
                        this.output += "(-";
                        this.emitExpr(expr.operand);
                        this.output += ")";
                        break;
                    case "not":
                        this.output += "(!";
                        this.emitExpr(expr.operand);
                        this.output += ")";
                        break;
                    case "address":
                        this.output += "(&";
                        this.emitExpr(expr.operand);
                        this.output += ")";
                        break;
                    case "deref":
                        this.output += "(*";
                        this.emitExpr(expr.operand);
                        this.output += ")";
                        break;
                    case "pre_inc":
                        this.output += "(++";
                        this.emitExpr(expr.operand);
                        this.output += ")";
                        break;
                    case "pre_dec":
                        this.output += "(--";
                        this.emitExpr(expr.operand);
                        this.output += ")";
                        break;
                    case "post_inc":
                        this.output += "(";
                        this.emitExpr(expr.operand);
                        this.output += "++)";
                        break;
                    case "post_dec":
                        this.output += "(";
                        this.emitExpr(expr.operand);
                        this.output += "--)";
                        break;
                }
                break;
            case "assign":
                this.emitExpr(expr.target);
                this.output += " = ";
                this.emitExpr(expr.value);
                break;
            case "call":
                this.emitExpr(expr.func);
                this.output += "(";
                for (let i = 0; i < expr.args.length; i++) {
                    if (i > 0) this.output += ", ";
                    this.emitExpr(expr.args[i]);
                }
                this.output += ")";
                break;
            case "index":
                this.emitExpr(expr.array);
                this.output += "[";
                this.emitExpr(expr.index);
                this.output += "]";
                break;
        }
    }
    emitStmt(stmt) {
        if (stmt.label && stmt.label !== "") {
            this.output += `${stmt.label}:
`;
        }
        switch (stmt.kind) {
            case "expr":
                this.indent();
                this.emitExpr(stmt.expr);
                this.output += ";\n";
                break;
            case "return":
                this.indent();
                this.output += "return";
                if (stmt.value) {
                    this.output += " ";
                    this.emitExpr(stmt.value);
                }
                this.output += ";\n";
                break;
            case "block":
                this.indent();
                this.output += "{\n";
                this.indentLevel++;
                for (const s of stmt.stmts) {
                    this.emitStmt(s);
                }
                this.indentLevel--;
                this.indent();
                this.output += "}\n";
                break;
            case "if":
                this.indent();
                this.output += "if (";
                this.emitExpr(stmt.cond);
                this.output += ")";
                if (stmt.thenStmt.kind === "block") {
                    this.output += " ";
                    this.emitStmtInline(stmt.thenStmt);
                } else {
                    this.output += " {\n";
                    this.indentLevel++;
                    this.emitStmt(stmt.thenStmt);
                    this.indentLevel--;
                    this.indent();
                    this.output += "}";
                }
                if (stmt.elseStmt) {
                    this.output += " else";
                    if (stmt.elseStmt.kind === "block" || stmt.elseStmt.kind === "if") {
                        this.output += " ";
                        this.emitStmtInline(stmt.elseStmt);
                    } else {
                        this.output += " {\n";
                        this.indentLevel++;
                        this.emitStmt(stmt.elseStmt);
                        this.indentLevel--;
                        this.indent();
                        this.output += "}\n";
                    }
                } else {
                    this.output += "\n";
                }
                break;
            case "while":
                this.indent();
                this.output += "while (";
                this.emitExpr(stmt.cond);
                this.output += ")";
                if (stmt.body.kind === "block") {
                    this.output += " ";
                    this.emitStmtInline(stmt.body);
                } else {
                    this.output += " {\n";
                    this.indentLevel++;
                    this.emitStmt(stmt.body);
                    this.indentLevel--;
                    this.indent();
                    this.output += "}\n";
                }
                break;
            case "for":
                this.indent();
                this.output += "for (";
                if (stmt.init) {
                    if (stmt.init.kind === "decl") {
                        this.emitDeclInline(stmt.init.decl);
                    } else if (stmt.init.kind === "expr") {
                        this.emitExpr(stmt.init.expr);
                    }
                }
                this.output += "; ";
                if (stmt.cond) {
                    this.emitExpr(stmt.cond);
                }
                this.output += "; ";
                if (stmt.update) {
                    this.emitExpr(stmt.update);
                }
                this.output += ")";
                if (stmt.body.kind === "block") {
                    this.output += " ";
                    this.emitStmtInline(stmt.body);
                } else {
                    this.output += " {\n";
                    this.indentLevel++;
                    this.emitStmt(stmt.body);
                    this.indentLevel--;
                    this.indent();
                    this.output += "}\n";
                }
                break;
            case "goto":
                this.indent();
                this.output += `goto ${stmt.target};
`;
                break;
            case "empty":
                break;
            case "decl":
                this.indent();
                this.emitDeclInline(stmt.decl);
                this.output += ";\n";
                break;
        }
    }
    emitStmtInline(stmt) {
        switch (stmt.kind) {
            case "block":
                this.output += "{\n";
                this.indentLevel++;
                for (const s of stmt.stmts) {
                    this.emitStmt(s);
                }
                this.indentLevel--;
                this.indent();
                this.output += "}\n";
                break;
            case "if":
                this.output += "if (";
                this.emitExpr(stmt.cond);
                this.output += ")";
                if (stmt.thenStmt.kind === "block") {
                    this.output += " ";
                    this.emitStmtInline(stmt.thenStmt);
                } else {
                    this.output += " {\n";
                    this.indentLevel++;
                    this.emitStmt(stmt.thenStmt);
                    this.indentLevel--;
                    this.indent();
                    this.output += "}";
                }
                if (stmt.elseStmt) {
                    this.output += " else";
                    if (stmt.elseStmt.kind === "block" || stmt.elseStmt.kind === "if") {
                        this.output += " ";
                        this.emitStmtInline(stmt.elseStmt);
                    } else {
                        this.output += " {\n";
                        this.indentLevel++;
                        this.emitStmt(stmt.elseStmt);
                        this.indentLevel--;
                        this.indent();
                        this.output += "}\n";
                    }
                } else {
                    this.output += "\n";
                }
                break;
            default:
                this.output += "{\n";
                this.indentLevel++;
                this.emitStmt(stmt);
                this.indentLevel--;
                this.indent();
                this.output += "}\n";
                break;
        }
    }
    emitDeclInline(decl) {
        switch (decl.kind) {
            case "var":
                this.emitType(decl.type);
                this.output += ` ${decl.name}`;
                if (decl.type.kind === "array") {
                    this.emitArrayDimensions(decl.type);
                }
                if (decl.init) {
                    this.output += " = ";
                    this.emitExpr(decl.init);
                }
                break;
            case "func":
                break;
        }
    }
    emitDecl(decl) {
        switch (decl.kind) {
            case "var":
                this.emitType(decl.type);
                this.output += ` ${decl.name}`;
                if (decl.type.kind === "array") {
                    this.emitArrayDimensions(decl.type);
                }
                if (decl.init) {
                    this.output += " = ";
                    this.emitExpr(decl.init);
                }
                this.output += ";\n";
                break;
            case "func":
                if (decl.type.kind === "function") {
                    this.emitType(decl.type.returnType);
                } else {
                    this.emitType(decl.type);
                }
                this.output += ` ${decl.name}(`;
                for (let i = 0; i < decl.params.length; i++) {
                    if (i > 0) this.output += ", ";
                    const param = decl.params[i];
                    this.emitType(param.type);
                    this.output += ` ${param.name}`;
                    if (param.type.kind === "array") {
                        this.output += "[]";
                    }
                }
                this.output += ")";
                if (decl.body) {
                    this.output += " {\n";
                    this.indentLevel = 1;
                    if (decl.body.kind === "block") {
                        for (const s of decl.body.stmts) {
                            this.emitStmt(s);
                        }
                    } else {
                        this.emitStmt(decl.body);
                    }
                    this.indentLevel = 0;
                    this.output += "}\n";
                } else {
                    this.output += ";\n";
                }
                break;
        }
    }
    emitProgram(program) {
        this.output = "// C code emitted from AST\n\n";
        for (const decl of program.declarations) {
            this.emitDecl(decl);
            this.output += "\n";
        }
        return this.output;
    }
};
function astPrint(program) {
    const printer = new AstPrinter();
    return printer.printProgram(program);
}
function emitC(program) {
    const emitter = new CEmitter();
    emitter.indentLevel = 0;
    return emitter.emitProgram(program);
}
var IRPrinter = class {
    constructor() {
        this.output = "";
        this.isSSA = false;
        this.subscripts = "\u2080\u2081\u2082\u2083\u2084\u2085\u2086\u2087\u2088\u2089";
    }
    detectSSA(program) {
        for (const func of program.functions) {
            if (func.params.some((p) => p.version > 0)) {
                return true;
            }
            for (const [_, block] of func.blocks) {
                if (block.instructions.some((inst) => inst.op === "phi")) {
                    return true;
                }
            }
        }
        return false;
    }
    toSubscript(n) {
        return n.toString().split("").map((d) => this.subscripts[parseInt(d)]).join("");
    }
    printProgram(program) {
        this.output = "";
        this.isSSA = this.detectSSA(program);
        for (const func of program.functions) {
            this.printFunction(func);
            this.output += "\n";
        }
        return this.output;
    }
    printFunction(func) {
        this.output += `function ${func.name}(`;
        this.output += func.params.map((p) => this.printVReg(p)).join(", ");
        this.output += ") {\n";
        const sortedBlocks = Array.from(func.blocks.entries()).sort((a, b) => {
            if (a[0] === func.entryBlock) return -1;
            if (b[0] === func.entryBlock) return 1;
            return a[0] - b[0];
        });
        for (const [label, block] of sortedBlocks) {
            this.printBlock(block, label === func.entryBlock);
        }
        this.output += "}\n";
    }
    printBlock(block, isEntry) {
        if (isEntry) {
            this.output += "  entry:\n";
        } else {
            this.output += `  L${block.label}:
`;
        }
        if (this.isSSA) {
            const phis = block.instructions.filter((inst) => inst.op === "phi");
            const regular = block.instructions.filter((inst) => inst.op !== "phi");
            if (phis.length > 0) {
                for (const inst of phis) {
                    this.output += "    ";
                    this.printInstruction(inst);
                    this.output += "\n";
                }
                if (regular.length > 0) {
                    this.output += "\n";
                }
            }
            for (const inst of regular) {
                this.output += "    ";
                this.printInstruction(inst);
                this.output += "\n";
            }
        } else {
            for (const inst of block.instructions) {
                this.output += "    ";
                this.printInstruction(inst);
                this.output += "\n";
            }
        }
        this.output += "    ";
        this.printTerminator(block.terminator);
        this.output += "\n";
    }
    printVReg(vreg) {
        switch (vreg.kind) {
            case "var":
                if (this.isSSA && vreg.version !== void 0) {
                    return `${vreg.sourceName}${this.toSubscript(vreg.version)}`;
                }
                return vreg.sourceName;
            case "param":
                if (this.isSSA && vreg.version !== void 0) {
                    return `${vreg.sourceName}${this.toSubscript(vreg.version)}`;
                }
                return vreg.sourceName;
            case "temp":
                return `t${vreg.id}`;
            case "addr":
                return `&${vreg.sourceName}`;
        }
    }
    printInstruction(inst) {
        switch (inst.op) {
            case "phi":
                this.output += `${this.printVReg(inst.dst)} = \u03C6(`;
                const sources = inst.sources.map(
                    (s) => `${this.printVReg(s.vreg)} from L${s.pred}`
                );
                this.output += sources.join(", ");
                this.output += ")";
                break;
            case "mov":
                this.output += `${this.printVReg(inst.dst)} = ${this.printVReg(inst.src)}`;
                break;
            case "loadi":
                this.output += `${this.printVReg(inst.dst)} = ${inst.imm}`;
                break;
            case "add":
            case "sub":
            case "mul":
            case "div":
            case "mod":
                this.output += `${this.printVReg(inst.dst)} = ${inst.op} `;
                this.output += `${this.printVReg(inst.lhs)}, ${this.printVReg(inst.rhs)}`;
                break;
            case "cmp":
                this.output += `${this.printVReg(inst.dst)} = cmp ${inst.cond} `;
                this.output += `${this.printVReg(inst.lhs)}, ${this.printVReg(inst.rhs)}`;
                break;
            case "alloca":
                this.output += `${this.printVReg(inst.dst)} = alloca ${inst.size}`;
                break;
            case "load":
                this.output += `${this.printVReg(inst.dst)} = load ${this.printVReg(inst.ptr)}`;
                break;
            case "store":
                this.output += `store ${this.printVReg(inst.ptr)}, ${this.printVReg(inst.value)}`;
                break;
            case "call":
                if (inst.dst !== void 0) {
                    this.output += `${this.printVReg(inst.dst)} = `;
                }
                this.output += `call ${inst.func}(`;
                this.output += inst.args.map((a) => this.printVReg(a)).join(", ");
                this.output += ")";
                break;
        }
    }
    printTerminator(term) {
        switch (term.op) {
            case "jump":
                this.output += `jump L${term.target}`;
                break;
            case "branch":
                this.output += `branch ${this.printVReg(term.cond)}, L${term.trueTarget}, L${term.falseTarget}`;
                break;
            case "ret":
                this.output += "ret";
                if (term.value !== void 0) {
                    this.output += ` ${this.printVReg(term.value)}`;
                }
                break;
        }
    }
};
function printIr(program) {
    const printer = new IRPrinter();
    return printer.printProgram(program);
}

// src/programs.ts
var sccp_test_function = `

int sccp_test() {
    int x = 1;
    int y = x + 0;
    int z = y;
    int w = y * x;
    int unused = x - x;
    if (x > z) {
        z = 42;
    } else {
        z = 8;
    }
    return z;
}
`;
var matmul = `
void matmul(){
    int c[16];
    int a[16];
    int b[16];
    int n = 4;


    int i = 0;
    while (i < n) {
        int j = 0;
        while (j < n) {
            int sum = 0;
            int k = 0;
            while (k < n) {
                sum = sum + a[i * n + k] * b[k * n + j];
                k++;
            }
            c[i * n + j] = sum;
            j++;
        }
        i++;
    }
}

    `;
var individualFunctions = {
    sccp_test: sccp_test_function,
    matmul
    // sieve: sieve,
    // quickSort: quickSort,
    // swap: swap_function,
    // array_ops: array_ops_function,
    // factorial: factorial_function,
    // fibonacci: fibonacci_function,
    // copy_chain: copy_chain_function,
    // algebra_test: algebra_test_function,
    // cse_test: cse_test_function,
    // constant_test: constant_test_function,
    // dead_branch_test: dead_branch_test_function,
    // phi_test: phi_test_function,
    // nested_loops: nested_loops_function,
    // complex_array_access: complex_array_access_function,
};
var everything = Object.values(individualFunctions).join("\n");
var testPrograms = {
    // everything: everything,
    ...individualFunctions
};

// src/registers.ts
function buildInterferenceGraph(func, liveness) {
    const graph = {
        nodes: /* @__PURE__ */ new Set(),
        edges: /* @__PURE__ */ new Map(),
        degree: /* @__PURE__ */ new Map()
    };
    for (const vregId of liveness.liveRanges.keys()) {
        graph.nodes.add(vregId);
        graph.edges.set(vregId, /* @__PURE__ */ new Set());
        graph.degree.set(vregId, 0);
    }
    for (let i = 0; i < func.params.length; i++) {
        for (let j = i + 1; j < func.params.length; j++) {
            addInterference(graph, func.params[i].id, func.params[j].id);
        }
    }
    for (const [label, block] of func.blocks) {
        const liveOut = liveness.liveOut.get(label);
        let currentlyLive = /* @__PURE__ */ new Set();
        for (const vreg of liveOut) {
            currentlyLive.add(vreg.id);
        }
        for (let i = block.instructions.length - 1; i >= 0; i--) {
            const inst = block.instructions[i];
            const def = instGetDef(inst);
            if (def) {
                for (const liveId of currentlyLive) {
                    if (liveId !== def.id) {
                        addInterference(graph, def.id, liveId);
                    }
                }
                currentlyLive.delete(def.id);
            }
            for (const use of instGetUses(inst)) {
                currentlyLive.add(use.id);
            }
        }
    }
    return graph;
}
function addInterference(graph, v1, v2) {
    if (v1 === v2) return;
    if (!graph.edges.get(v1).has(v2)) {
        graph.edges.get(v1).add(v2);
        graph.edges.get(v2).add(v1);
        graph.degree.set(v1, (graph.degree.get(v1) || 0) + 1);
        graph.degree.set(v2, (graph.degree.get(v2) || 0) + 1);
    }
}
function createARM64RegisterInfo() {
    const names = /* @__PURE__ */ new Map();
    for (let i = 0; i <= 28; i++) {
        names.set(i, `x${i}`);
    }
    const stolen = [8, 7, 6];
    return {
        /*
        x0-x7 reserved for ABI (parameters, call args, returns)
        but because theres not spilling, and a few functions are register heavy, I've
        also had to steal 8 through 6
        x8 reserved (indirect result location)
        x16-x18 reserved (intra-procedure-call scratch)
        */
        callerSaved: [9, 10, 11, 12, 13, 14, 15, ...stolen],
        calleeSaved: [19, 20, 21, 22, 23, 24, 25, 26, 27, 28],
        names
    };
}
function allocateRegistersForFunction(func, liveness, graph, regInfo) {
    const allocation = /* @__PURE__ */ new Map();
    const usedCalleeSaved = /* @__PURE__ */ new Set();
    const unallocated = Array.from(graph.nodes).map((id) => ({
        id,
        degree: graph.degree.get(id) || 0,
        vreg: liveness.liveRanges.get(id)?.vreg
    })).sort((a, b) => {
        if (b.degree !== a.degree) return b.degree - a.degree;
        return 0;
    });
    for (const node of unallocated) {
        const forbidden = /* @__PURE__ */ new Set();
        for (const neighborId of graph.edges.get(node.id) || []) {
            const neighborReg = allocation.get(neighborId);
            if (neighborReg !== void 0) {
                forbidden.add(neighborReg);
            }
        }
        const pool = regInfo.callerSaved;
        let selected = null;
        for (const reg of pool) {
            if (!forbidden.has(reg)) {
                selected = reg;
                break;
            }
        }
        assert(selected != null, "Out of registers");
        allocation.set(node.id, selected);
        if (regInfo.calleeSaved.includes(selected)) {
            usedCalleeSaved.add(selected);
        }
    }
    return { assignment: allocation, usedCalleeSaved };
}
function allocateRegisters(program) {
    const allocations = /* @__PURE__ */ new Map();
    const regInfo = createARM64RegisterInfo();
    for (const func of program.functions) {
        const cfg = program.cfg.functionCFGs.get(func.name);
        const liveness = computeLiveness(func, cfg);
        const graph = buildInterferenceGraph(func, liveness);
        const allocation = allocateRegistersForFunction(
            func,
            liveness,
            graph,
            regInfo
        );
        allocations.set(func.name, allocation);
    }
    return allocations;
}
