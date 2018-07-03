use Lkn.Prelude
alias Lkn.Physics.Geometry.Vector
alias Lkn.Physics.Geometry.Box

import Lkn.Core.Puppet, only: [defpuppet: 2]

defpuppet Lykan.Character do
  defmodule Combat do
    use Lykan.System.Combat.Component.Puppet

    def init_state(_key) do
      {:ok, nil}
    end

    def attack_zone(key, state) do
      vec = Lykan.System.Physics.Body.get_position(key)
      box = Lykan.System.Physics.Body.get_box(key)

      {{box, vec}, state}
    end
  end

  defmodule Body do
    use Lykan.System.Physics.Body

    def init_state(_key) do
      {:ok, nil}
    end

    def get_position(key, nil) do
      Option.some(x) = read(key, :x)
      Option.some(y) = read(key, :y)

      {Vector.new(x, y), nil}
    end

    def get_box(_key, nil) do
      {Box.new(24, 24), nil}
    end

    def set_position(key, pos, nil) do
      write(key, :x, pos.x)
      write(key, :y, pos.y)

      nil
    end

    def get_direction(key, nil) do
      Option.some(dir) = read(key, :direction)
      {dir, nil}
    end

    def set_direction(key, dir, _) do
      write(key, :direction, dir)
      nil
    end
  end

  defmodule Appearance do
    use Lykan.System.Appearance.Component

    def set_color(key, c, no_state) do
      write(key, :color, c)
      no_state
    end

    def get_color(key, no_state) do
      Option.some(c) = read(key, :color)
      {c, no_state}
    end

    def init_state(_key) do
      {:ok, :no_state}
    end
  end

  @components [Appearance, Body, Combat]

  def start_link(key) do
    Lkn.Core.Entity.start_link(__MODULE__, key, :no_args)
  end

  def init_properties(_no_args) do
    %{
      :color => "black",
      :x => 50,
      :y => 50,
      :direction => Math.pi / 2,
      :look_at => Math.pi / 2,
    }
  end

  def digest(props) do
    Map.drop(props, [:module])
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
        supervisor(Lykan.Character, [], restart: :transient)
      ]

      Supervisor.init(children, strategy: :simple_one_for_one)
    end
  end

  def spawn(key) do
    Supervisor.start_child(Lykan.Character.Sup, [key])
  end
end
