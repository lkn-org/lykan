use Lkn.Prelude

defmodule Renaissance.Puppeteer.Player do
  use Renaissance.Puppeteer do
  end

  def init_state(no_state) do
    {:ok, no_state}
  end

  def start_link(puppeteer_key) do
    Lkn.Core.Puppeteer.start_link(__MODULE__, puppeteer_key, :no_state)
  end

  def leave_instance(no_state, _instance_key) do
    no_state
  end

  def puppet_color(_key, puppet, c, no_state) do
    IO.puts "[Puppet(#{inspect puppet})] new color is #{c}"
    no_state
  end
end
