defmodule Lykan do
  import Supervisor.Spec

  defmodule Connect do
    alias Socket.Web

    def server(port) do
      # setup a set of maps from a config file
      conf = Lykan.Config.from_file!("lykan.json")
      Enum.map(conf.maps, fn m ->
          Lykan.Map.spawn(m, "blue")
          Lkn.Core.Pool.spawn_pool(m)
        end)

      # listen for incoming connection
      server = Web.listen!(port)
      loop_acceptor(server, conf.default_map)
    end

    defp loop_acceptor(server, map_key) do
      client = Web.accept!(server)

      task(fn -> serve(client, map_key) end)
      loop_acceptor(server, map_key)
    end

    defp serve(client, map_key) do
      Socket.Web.accept!(client)

      # spawn a player puppeteer and find an instance
      puppeteer_key = UUID.uuid4()
      Lykan.Puppeteer.Player.start_link(puppeteer_key, client)
      Lkn.Core.Puppeteer.find_instance(puppeteer_key, map_key)

      # spawn a character for this puppeteer
      chara_key = UUID.uuid4()
      Lykan.Character.spawn(chara_key)
      Lykan.Puppeteer.Player.assign_puppet(puppeteer_key, chara_key)

      recv(puppeteer_key, client)
    end

    defp recv(puppeteer_key, client) do
      case Web.recv(client) do
        {:ok, {:text, msg}} ->
          IO.puts "[Puppeteer(puppeteer_key)] received #{msg}"

          recv(puppeteer_key, client)
        _ ->
          Lykan.Puppeteer.Player.kill(puppeteer_key)
      end
    end

    defp task(lambda) do
      Task.Supervisor.start_child(Lykan.Tasks, lambda)
    end
  end

  def start(_type, _args) do
    children = [
      supervisor(Lykan.Map.Sup, [], restart: :transient),
      supervisor(Lykan.Character.Sup, [], restart: :transient),
      supervisor(Task.Supervisor, [[name: Lykan.Tasks]], restart: :transient),
      worker(Task, [Connect, :server, [4000]], restart: :transient),
    ]

    Supervisor.start_link(children,
      strategy: :one_for_one,
      name: Lykan)
  end
end
