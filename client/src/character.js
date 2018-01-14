import * as PIXI from 'pixi.js';

import { CHARACTER_WIDTH
       , CHARACTER_HEIGHT
       } from './constant.js';

let frames = {};

let lines = {
       "up": 0,
       "right": 1,
       "down": 2,
       "left": 3,
};

let columns = {
       "walk": 0,
       "attack": 3,
       "magic": 6,
};

let seq = [1, 2, 0];

function get_textures(asset) {
       if (!(asset in frames)) {
              let texture = PIXI.loader.resources[asset].texture;

              frames[asset] = {};
              for (var dir in lines) {
                     frames[asset][dir] = {};

                     for (var action in columns) {
                            frames[asset][dir][action] = [];

                            for (var offset in seq) {
                                   let newTexture = new PIXI.Texture(
                                          texture,
                                          new PIXI.Rectangle(
                                                 CHARACTER_WIDTH * (columns[action] + seq[offset]),
                                                 lines[dir] * CHARACTER_HEIGHT,
                                                 CHARACTER_WIDTH,
                                                 CHARACTER_HEIGHT
                                          )
                                   );

                                   frames[asset][dir][action].push(newTexture);
                            }
                     }
              }
       }

       return frames[asset];
}

export function get_textures_for(asset, dir, action) {
       return get_textures(asset)[dir][action];
}
