import * as PIXI from 'pixi.js';

import {
  CHARACTER_WIDTH,
  CHARACTER_HEIGHT,
} from './constant.json';

const frames = {};

const lines = {
  up: 0,
  right: 1,
  down: 2,
  left: 3,
};

const columns = {
  walk: 0,
  attack: 3,
  magic: 6,
};

const seq = [1, 2, 0];

function getTextures(asset) {
  if (!(asset in frames)) {
    const { texture } = PIXI.loader.resources[asset];

    frames[asset] = {};
    Object.keys(lines).forEach((dir) => {
      frames[asset][dir] = {};

      Object.keys(columns).forEach((action) => {
        frames[asset][dir][action] = [];

        Object.keys(seq).forEach((offset) => {
          const newTexture = new PIXI.Texture(
            texture,
            new PIXI.Rectangle(
              CHARACTER_WIDTH * (columns[action] + seq[offset]),
              lines[dir] * CHARACTER_HEIGHT,
              CHARACTER_WIDTH,
              CHARACTER_HEIGHT,
            ),
          );

          frames[asset][dir][action].push(newTexture);
        });
      });
    });
  }

  return frames[asset];
}

// eslint-disable-next-line import/prefer-default-export
export const getTexturesFor = (asset, dir, action) => getTextures(asset)[dir][action];
