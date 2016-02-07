"use strict";

// module Graphics.WebGL.Shader

exports.getAttrBindingsImpl = function getAttrBindingsImpl(ctx, prog, wrapper) {
  return function () {
    var all, attr, count, loc;

    try {
      all = {};
      count = ctx.getProgramParameter(prog, ctx.ACTIVE_ATTRIBUTES);

      for (var i = 0; i < count; i++) {
	attr = ctx.getActiveAttrib(prog, i);
	loc = ctx.getAttribLocation(prog, attr.name);
	all[attr.name] = wrapper(loc);
      }

      return all;
    } catch(e) {
      return null;
    }
  };
}


exports.getUniformBindingsImpl = function getUniformBindingsImpl(ctx, prog, wrapper) {
    return function () {
      var all, unif, count, loc;

      try {
        all = {};
        count = ctx.getProgramParameter(prog, ctx.ACTIVE_UNIFORMS);

        for (var i = 0; i < count; i++) {
          unif = ctx.getActiveUniform(prog, i);
          loc = ctx.getUniformLocation(prog, unif.name);
          all[unif.name] = wrapper(loc);
        }

        return all;
      } catch(e) {
        return null;
      }
    };
  }
