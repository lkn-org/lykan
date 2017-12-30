use Lkn.Prelude
import Lkn.Core.Map, only: [defmap: 2]

defmap Lykan.Map do
  defmodule World do
    use Lykan.System.Physics.World

    def boundaries(key, :no_state) do
      {{100, 100}, :no_state}
    end

    def init_state(_key) do
      {:ok, :no_state}
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

  def init_properties(c) do
    # no more than 5 players, and keep the instance alive one second after the
    # last player left
    %{
      :delay => 1000,
      :limit => 2,
      :default_color => c,
     }
  end

  def digest(props) do
    Map.drop(props, [:delay, :limit])
  end

  def destroy(_puppet_key, _puppet, _reason) do
    :ok
  end

  defmodule Sup do
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
