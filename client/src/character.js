import * as PIXI from 'pixi.js';

import { CHARACTER_WIDTH
       , CHARACTER_HEIGHT
       } from './constant.js';

let frames = {};
let lines = {};

lines["up"] = 0;
lines["right"] = 1;
lines["down"] = 2;
lines["left"] = 3;

frame_line(frames, "up");
frame_line(frames, "right");
frame_line(frames, "left");
frame_line(frames, "down");

function frame_line(res, dir) {
       res[dir] = new PIXI.Rectangle(
              CHARACTER_WIDTH,
              lines[dir] * CHARACTER_HEIGHT,
              CHARACTER_WIDTH,
              CHARACTER_HEIGHT
       )
}

export function get_frame(dir) {
       return frames[dir];
}
