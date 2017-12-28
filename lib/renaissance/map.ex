use Lkn.Prelude
import Lkn.Core.Map, only: [defmap: 2]

defmap Renaissance.Map do
  defmodule Appearance do
    use Renaissance.System.Appearance.Component

    def set_color(key, c, _state) do
      c
    end

    def get_color(key, c) do
      {c, c}
    end

    def init_state(key) do
      Option.some(c) = read(key, :default_color)

      {:ok, c}
    end
  end

  @components [Appearance]

  def start_link(key, c) do
    Lkn.Core.Entity.start_link(__MODULE__, key, c)
  end

  def init_properties(c) do
    # no more than 5 players, and keep the instance alive one second after the
    # last player left
    %{
      :delay => 1000,
      :limit => 5,
      :default_color => c,
     }
  end
end