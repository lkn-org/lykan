use Lkn.Prelude
import Lkn.Core.Map, only: [defmap: 2]
alias Lkn.Physics.Body
alias Lkn.Physics.Geometry.Box
alias Lkn.Physics.Geometry.Vector
alias Lykan.System.Physics.Teleporter

defmap Lykan.Map do
  defmodule World do
    use Lykan.System.Physics.World

    def boundaries(_key, boundaries) do
      {boundaries, boundaries}
    end

    def init_state(key) do
      Option.some(w) = read(key, :width)
      Option.some(h) = read(key, :height)

      {:ok, {w, h}}
    end

    def get_teleporters(key, st) do
      Option.some(t) = read(key, :teleporters)

      {t, st}
    end
  end

  defmodule Appearance do
    use Lykan.System.Appearance.Component

    def set_color(key, new_color, _c) do
      new_color
    end

    def get_color(key, c) do
      {c, c}
    end

    def init_state(key) do
      Option.some(c) = read(key, :default_color)

      {:ok, c}
    end
  end

  @components [Appearance, World]

  def start_link(key, c) do
    Lkn.Core.Entity.start_link(__MODULE__, key, c)
  end

  def init_properties(attrs) do
    # build the teleporters information
    tels = Enum.map(attrs["teleporters"], fn v ->
      Teleporter.new(
        Body.new(
          Vector.new(v["at"]["x"], v["at"]["y"]),
          Box.new(v["at"]["width"], v["at"]["height"])
        ),
        {v["target"]["map"], v["target"]["entry_point"]}
      )
    end)

    # no more than 5 players, and keep the instance alive one second after the
    # last player left
    %{
      :delay => 1000,
      :limit => 5,
      :default_color => attrs["color"],
      :width => attrs["width"],
      :height => attrs["height"],
      :entry_points => attrs["entry_point"],
      :teleporters => tels,
     }
  end

  def digest(props) do
    Map.drop(props, [:delay, :limit, :teleporters, :entry_points])
  end

  def destroy(_puppet_key, _puppet, _reason) do
    :ok
  end

  defmodule Sup do
    @moduledoc false
    use Supervisor

    def start_link do
      Supervisor.start_link(__MODULE__, :no_arg, name: __MODULE__)
    end

    def init(_no_arg) do
      children = [
        supervisor(Lykan.Map, [], restart: :transient)
      ]

      Supervisor.init(children, strategy: :simple_one_for_one)
    end
  end

  def spawn(map_key, color) do
    Supervisor.start_child(Lykan.Map.Sup, [map_key, color])
  end
end
