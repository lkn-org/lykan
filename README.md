# lykan

*lykan* is a 2D, online, role playing game server written in Elixir using the
[lkn](https://hex.pm/packages/lkn_core) game engine. It is a work in progress
and there is not much to see.

## Getting Started

### Compiling From Source

The *lykan* codebase is currently hosted on Github.

```
mkdir lkn/
cd lkn
git clone https://github.com/lkn-org/lykan
```

*lykan* uses the upstream versions of `lkn-repo` and `lkn-physics`, so
you need to clone these repositories too.

```
cd ..
git clone https://github.com/lkn-org/lkn-physics
git clone https://github.com/lkn-org/lkn-repo
cd lykan
```

If everything went fine, you are good to go:

```
cd lykand
mix deps.get
mix compile
```

### Setting Up Databases

The `lykan_repo` library is responsible for correctly setting up the databases
and provided the related Elixir functions to manipulate them. The only thing it
requires is a user (`lykan_dev` in dev mode) with enough privileges to create
databases, tables and users.

```
cd lykan-repo
mix deps.get
mix ecto.create
mix ecto.migrate
```

Currently, this is not used.

### Compiling From Source

The *lykan* repository brings a file called `lykan.json`. You can see it as some
sort of “game project”, as *lykan* is still pretty generic. The `:lykan`
application will try to read `lykan.json` at startup, so this file needs to be
readable by the application. You can set the full path to this file as the
environment variable `LYKAN_CONFIG_FILE`

Once it is done, you can start the daemon:

```
cd lykand
iex -S mix
```

You will then need to build the client application and serve it. Here is how
to achieve this for development purpose:

```
cd client
npm install
npm run start:dev
```

It will compile the JavaScript client and open a browser with `client.html`,
connecting you to the game, and letting you “play.”

Note that the current plan is to implement a native client in Lisp
(because why not?). The project is called `lykanc`.
