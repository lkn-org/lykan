use Lkn.Prelude

import Lkn.Core.System, only: [defsystem: 2]
import Lkn.Core.Component, only: [defcomponent: 2]

defsystem Lykan.System.Physics do
  @type direction :: :up | :down | :right | :left
  @type position :: {non_neg_integer, non_neg_integer}

  defcomponent Body do
    @system Lykan.System.Physics

    @call get_direction() :: Lykan.System.Physics.direction
    @cast set_direction(dir :: Lykan.System.Physics.direction)

    @call get_position() :: Lkn.System.Physics.position
    @cast set_position(pos :: Lkn.System.Physics.position)
  end

  defcomponent World do
    @system Lykan.System.Physics

    @call boundaries() :: {non_neg_integer, non_neg_integer}
  end

  @map World
  @puppet Body

  #############################################################################
  def init_state(instance_key, _map_key) do
    Map.new()
  end

  def puppet_enter(state, instance_key, map_key, puppets, puppet_key) do
    state
  end

  def puppet_leave(state, instance_key, map_key, puppets, puppet_key) do
    state
  end

  #############################################################################
  cast puppet_starts_moving(puppet_key :: Lkn.Core.Puppet.k) do
    {:ok, beac} = Beacon.start(instance_key)
    beac |> Beacon.set_periodic_callback(300, &Lykan.System.Physics.puppet_moves(&1, puppet_key))
         |> Beacon.enable()

    dir = Body.get_direction(puppet_key)
    pos = Body.get_position(puppet_key)

    notify(&Lykan.Puppeteer.notify(&1, {:puppet_starts_moving, puppet_key, dir, pos}))

    Map.put(state, puppet_key, beac)
  end

  cast puppet_moves(puppet_key :: Lkn.Core.Puppet.k) do
    dir = Body.get_direction(puppet_key)
    pos = Body.get_position(puppet_key)
    bound = World.boundaries(map_key)

    pos = moves(pos, bound, dir)

    Body.set_position(puppet_key, pos)

    notify(&Lykan.Puppeteer.notify(&1, {:puppet_moves, puppet_key, dir, pos}))

    state
  end

  cast puppet_stops_moving(puppet_key :: Lkn.Core.Puppet.k) do
    case Map.fetch(state, puppet_key) do
      {:ok, beac} ->
        Beacon.cancel(beac)

        pos = Body.get_position(puppet_key)
        dir = Body.get_direction(puppet_key)

        notify(&Lykan.Puppeteer.notify(&1, {:puppet_stops_moving, puppet_key, pos}))

        Map.delete(state, puppet_key)
      _ ->
        state
    end
  end

  defp moves({x, y}, {max_x, max_y}, :up) do
    {x, min(max_y, y + 5)}
  end
  defp moves({x, y}, {max_x, max_y}, :down) do
    {x, max(0, y - 5)}
  end
  defp moves({x, y}, {max_x, max_y}, :left) do
    {max(0, x - 5), y}
  end
  defp moves({x, y}, {max_x, max_y}, :right) do
    {min(max_x, x + 5), y}
  end
end
