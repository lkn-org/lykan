defmodule Renaissance do
  @moduledoc """
  Documentation for Renaissance.
  """

  def hello(m, p, c, col) do
    Renaissance.Map.start_link(m, col)
    Lkn.Core.Pool.spawn_pool(m)
    Renaissance.Puppeteer.Player.start_link(p)
    Renaissance.Character.start_link(c)
    instance_key = Lkn.Core.Puppeteer.find_instance(p, m)
    Lkn.Core.Instance.register_puppet(instance_key, c)
  end
end
