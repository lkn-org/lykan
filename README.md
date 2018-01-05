# lykan

*lykan* is a 2D, online, role playing game server written in Elixir using the
[lkn](https://hex.pm/packages/lkn_core) game engine. It is a work in progress
and there is not much to see.

## Getting Started

*lykan* uses `pijul`, a young version control system written in rust.

```
mkdir lkn/
cd lkn
pijul clone https://nest.pijul.com/lthms/lykan
```

*lykan* uses the upstream versions of `lkn-core` and `lkn-physics`, so you need
to clone these repositories too.

```
pijul clone https://nest.pijul.com/lthms/lkn-physics
pijul clone https://nest.pijul.com/lthms/lkn-core
```

If everything went fine, you are good to go:

```
cd lykan
mix deps.get
mix compile
```

The *lykan* repository brings a file called `lykan.json`. You can see it as some
sort of “game project”, as *lykan* is still pretty generic. The `:lykan`
application will try to read `/opt/lykan/lykan.json` at startup, so you need to
create this repository. You may need a root access to your machine to do that;
in such a case, don't forget to correctly set the read permission so that your
process will be able to read it.

Once it is done, you can start the daemon:

```
iex -S mix
```

Use your favorite browser to display the `client/client.html` webpages. It will
connect you to the game, and you will be able to “play.”

![client screenshot](client/screenshot.png)
