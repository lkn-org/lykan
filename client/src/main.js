// Imports
import * as PIXI from 'pixi.js';
import 'pixi-tiledmap';

import * as Character from './character';
import {
  SCREEN_WIDTH,
  SCREEN_HEIGHT,
  CHARACTER_HEIGHT,
  CHARACTER_WIDTH,
  TILE_SIZE,
} from './constant.json';

// Application to draw things
const app = new PIXI.Application({
  width: SCREEN_WIDTH,
  height: SCREEN_HEIGHT,
  antialias: false,
  resolution: 1,
});

document.body.appendChild(app.view);

// Keyboard keys handlers
const keys = {};

// Websocket from the server
let ws;

// Game state
const state = {
  camera: {},
  scene: {},
  map: {
    width: 0,
    height: 0,
  },
  puppets: {},
  main: {},
  controls: [],
  in_game: false,
};

function centerCamera() {
  const pk = state.main;

  const x = (SCREEN_WIDTH / 2) - state.puppets[pk].x;
  const y = (SCREEN_HEIGHT / 2) - state.puppets[pk].y;

  state.camera.position.x = x;
  state.camera.position.y = y;
}

function removePuppet(pk) {
  state.scene.layers.objects.removeChild(state.puppets[pk]);
  state.puppets[pk] = null;
}

function placePuppet(pk, x, y) {
  const mapHeight = state.map.height;
  const puppetHeight = state.puppets[pk].height;

  state.puppets[pk].x = x;
  state.puppets[pk].y = mapHeight - y - puppetHeight;
}

function addNewPuppet(pk, digest) {
  const textures = Character.getTexturesFor('assets/character.png', 'down', 'walk');

  state.puppets[pk] = new PIXI.extras.AnimatedSprite(textures);

  state.puppets[pk].isMoving = false;
  state.puppets[pk].animationSpeed = 0.08;
  state.puppets[pk].width = CHARACTER_WIDTH;
  state.puppets[pk].height = CHARACTER_HEIGHT;
  placePuppet(pk, digest.x, digest.y);

  state.scene.layers.objects.addChild(state.puppets[pk]);

  if (pk === state.main) {
    centerCamera();
  }
}

function listenWS(event) {
  const { opcode, message } = JSON.parse(event.data);

  switch (opcode) {
    case 'ATTRIBUTE_PUPPET': {
      // we know who we are going to play
      state.main = message.puppet_key;
      break;
    }
    case 'LEAVE_MAP': {
      state.in_game = false;
      break;
    }
    case 'INSTANCE_DIGEST': {
      // we have joined a new instance, first let us remove all previous puppets
      Object.keys(state.puppets).filter(Boolean).forEach(removePuppet);

      // then display the map
      const mapTmx = `assets/${message.map.map_key}.tmx`;
      state.camera.removeChild(state.scene);
      state.scene = new PIXI.extras.TiledMap(mapTmx);
      state.camera.addChild(state.scene);

      Object.keys(keys).forEach((keyCode) => {
        if (keys[keyCode].isDown) {
          keys[keyCode].release();
          keys[keyCode].press();
        }
      });
      state.map.width = message.map.digest.width * TILE_SIZE;
      state.map.height = message.map.digest.height * TILE_SIZE;

      // then display the puppets already inside
      Object.keys(message.puppets).filter(Boolean).forEach((pk) => {
        addNewPuppet(pk, message.puppets[pk]);
      });

      // we can display our scene now
      state.camera.visible = true;
      state.in_game = true;

      // we now need to see if the player wants to move, by looking at
      // state.controls.
      if (state.controls.length >= 1) {
        ws.send('MOVE');
        ws.send(state.controls[state.controls.length - 1]);
      }
      break;
    }
    case 'PUPPET_STARTS': {
      state.puppets[message.puppet_key].isMoving = true;
      state.puppets[message.puppet_key].gotoAndPlay(0);
      break;
    }
    case 'PUPPET_STOPS': {
      state.puppets[message.puppet_key].isMoving = false;
      state.puppets[message.puppet_key].gotoAndStop(0);
      break;
    }
    case 'PUPPET_ENTERS': {
      addNewPuppet(message.puppet_key, message.digest);
      break;
    }
    case 'PUPPET_LEAVES': {
      removePuppet(message.puppet_key, message.digest);
      break;
    }
    case 'PUPPET_MOVES': {
      const pk = message.puppet_key;

      placePuppet(pk, message.position.x, message.position.y);

      if (pk === state.main) {
        centerCamera();
      }
      break;
    }
    case 'PUPPET_DIRECTION': {
      state.puppets[message.puppet_key].textures =
        Character.getTexturesFor(
          'assets/character.png',
          message.direction,
          'walk',
        );

      if (state.puppets[message.puppet_key].isMoving) {
        state.puppets[message.puppet_key].gotoAndPlay(0);
      }
      break;
    }
    default: break;
  }
}

function keyboard(keyCode) {
  const key = {
    code: keyCode,
    isDown: false,
    isUp: true,
    press: undefined,
    release: undefined,
    downHandler: (event) => {
      if (event.keyCode === key.code) {
        if (key.isUp && key.press) {
          key.press();
          event.preventDefault();
        }
        key.isDown = true;
        key.isUp = false;
      }
    },
    upHandler: (event) => {
      if (event.keyCode === key.code) {
        if (key.isDown && key.release) {
          key.release();
          event.preventDefault();
        }
        key.isDown = false;
        key.isUp = true;
      }
    },
  };

  // Attach event listeners
  window.addEventListener('keydown', key.downHandler, false);
  window.addEventListener('keyup', key.upHandler, false);
  return key;
}

function setupKey(code, string) {
  const handler = keyboard(code);

  handler.press = () => {
    state.controls.push(string);

    // we are in a map, so we notify the server according to the state of
    // state.controls.
    if (state.in_game) {
      // at least we chane our puppet direction
      ws.send(string);

      // and if no key was pressed before, we start moving
      if (state.controls.length === 1) {
        ws.send('MOVE');
      }
    }
  };

  handler.release = () => {
    state.controls = state.controls.filter(x => x !== string);

    // we are in a map, so we notify the server according to the state of
    // state.controls.
    if (state.in_game) {
      if (!state.controls.length) {
        // if the stack of controls is empty, this means we have to stop
        ws.send('STOP');
      } else {
        // otherwise, we take the top of the stack as the new direction
        ws.send(state.controls[state.controls.length - 1]);
      }
    }
  };

  keys[code] = handler;
}

function layerSorter(a, b) {
  const y1 = a.y;
  const y2 = b.y;

  if (y1 < y2) {
    return -1;
  } else if (y1 > y2) {
    return 1;
  }
  return 0;
}

function gameLoop(/* delta */) {
  // reorder children so that upper entities appear behind lower ones
  if ('layers' in state.scene) {
    state.scene.layers.objects.children.sort(layerSorter);
  }
}

function setup() {
  // init websocket listening
  ws = new WebSocket('ws://localhost:4000');
  ws.onmessage = listenWS;

  // setup the keyboard
  setupKey(37, 'LEFT');
  setupKey(38, 'UP');
  setupKey(39, 'RIGHT');
  setupKey(40, 'DOWN');

  // camera container
  state.camera = new PIXI.Container();
  app.stage.addChild(state.camera);
  state.camera.visible = true;

  // init game loop
  app.ticker.add(delta => gameLoop(delta));
}

PIXI.loader
  .add('assets/character.png')
  .add('assets/m1.tmx')
  .add('assets/m2.tmx')
  .load(setup);
