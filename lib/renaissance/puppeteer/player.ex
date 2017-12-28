use Lkn.Prelude

defmodule Renaissance.Puppeteer.Player do
  use Renaissance.Puppeteer do
  end

  def init_state(socket) do
    {:ok, socket}
  end

  def start_link(puppeteer_key, socket) do
    Lkn.Core.Puppeteer.start_link(__MODULE__, puppeteer_key, socket)
  end

  def leave_instance(no_state, _instance_key) do
    no_state
  end

  def puppet_color(_key, puppet, c, socket) do
    IO.puts "[Puppet(#{inspect puppet})] new color is #{c}"
    socket
  end
end
