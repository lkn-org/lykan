defmodule Lykan do
  import Supervisor.Spec

  defmodule Game do
    alias Socket.Web

    def server(port) do
      # setup a set of maps from a config file
      conf = Lykan.Config.from_env!("lykan.json")
      Enum.map(conf.maps, fn {m, attrs} ->
          Lykan.Map.spawn(m, attrs)
          Lkn.Core.Pool.spawn_pool(m)
        end)

      # listen for incoming connection
      server = Web.listen!(port)
      loop_acceptor(server, conf.default_map)
    end

    defp loop_acceptor(server, map_key) do
      client = Web.accept!(server)

      task(fn -> Lykan.Puppeteer.Player.accept(client, map_key) end)
      loop_acceptor(server, map_key)
    end

    defp task(lambda) do
      Task.Supervisor.start_child(Lykan.Tasks, lambda)
    end
  end

  def start(_type, _args) do
    game_port = Application.get_env(:lykand, :game_port)

    children = [
      supervisor(Lykan.Repo, [[name: Lykan.Repo]], restart: :transient),
      supervisor(Lykan.Map.Sup, [], restart: :transient),
      supervisor(Lykan.Character.Sup, [], restart: :transient),
      supervisor(Task.Supervisor, [[name: Lykan.Tasks]], restart: :transient),
      worker(Task, [Game, :server, [game_port]], restart: :transient),
    ]

    Supervisor.start_link(children,
      strategy: :one_for_one,
      name: Lykan)
  end
end
