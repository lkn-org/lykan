defmodule Renaissance do
  import Supervisor.Spec

  defmodule Connect do
    alias Socket.Web

    def server(port) do
      # setup the bare minimum for one map
      map_key = UUID.uuid4()
      Renaissance.Map.start_link(map_key, "blue")
      Lkn.Core.Pool.spawn_pool(map_key)

      # listen for incoming connection
      server = Web.listen!(port)
      loop_acceptor(server, map_key)
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
      Renaissance.Puppeteer.Player.start_link(puppeteer_key, client)
      instance_key = Renaissance.Puppeteer.Player.goto(puppeteer_key, map_key)

      # spawn a character for this puppeteer
      chara_key = UUID.uuid4()
      Renaissance.Character.start_link(chara_key)
      Renaissance.Puppeteer.Player.assign_puppet(puppeteer_key, chara_key)

      recv(puppeteer_key, client)
    end

    defp recv(puppeteer_key, client) do
      case Web.recv(client) do
        {:ok, {:text, msg}} ->
          IO.puts "[Puppeteer(puppeteer_key)] received #{msg}"

          recv(puppeteer_key, client)
        _ ->
          Renaissance.Puppeteer.Player.kill(puppeteer_key)
      end
    end

    defp task(lambda) do
      Task.Supervisor.start_child(Renaissance.Tasks, lambda)
    end
  end

  def start(_type, _args) do
    children = [
      supervisor(Task.Supervisor, [[name: Renaissance.Tasks]], restart: :transient),
      worker(Task, [Connect, :server, [4000]], restart: :transient),
    ]

    Supervisor.start_link(children,
      strategy: :one_for_one,
      name: Renaissance)
  end
end
