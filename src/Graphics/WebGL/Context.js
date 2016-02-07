"use strict";

// module Graphics.WebGL.Context

exports.getWebglContextWithAttrsImpl = function getWebglContextWithAttrsImpl(canvas, attrs, Just, Nothing) {
  return function () {
    try {
      return Just(
        canvas.getContext('webgl', attrs) || canvas.getContext('experimental-webgl', attrs)
      );
    } catch(err) {
      return Nothing;
    };
  }
}
