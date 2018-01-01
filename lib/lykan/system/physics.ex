use Lkn.Prelude

import Lkn.Core.System, only: [defsystem: 2]
import Lkn.Core.Component, only: [defcomponent: 2]
import Lykan.Message, only: [defmessage: 2]

defsystem Lykan.System.Physics do
  defmodule Position do
    defstruct [
      :x,
      :y,
    ]

    @type t :: %Position{
      x: non_neg_integer,
      y: non_neg_integer,
    }

    @spec new(x :: non_neg_integer, y :: non_neg_integer) :: t
    def new(x, y) do
      %Position{
        x: x,
        y: y,
      }
    end
  end

  @type direction :: :up | :down | :right | :left

  defcomponent Body do
    @system Lykan.System.Physics

    @call get_direction() :: Lykan.System.Physics.direction
    @cast set_direction(dir :: Lykan.System.Physics.direction)

    @call get_position() :: Lkn.System.Physics.Position.t
    @cast set_position(pos :: Lkn.System.Physics.Position.t)
  end

  defcomponent World do
    @system Lykan.System.Physics

    @call boundaries() :: {non_neg_integer, non_neg_integer}
  end

  @map World
  @puppet Body

  #############################################################################

  defmessage PuppetStarts do
    opcode "PUPPET_STARTS"

    content [
      :puppet_key,
      :direction,
      :position,
    ]
  end

  defmessage PuppetMoves do
    opcode "PUPPET_MOVES"

    content [
      :puppet_key,
      :direction,
      :position,
    ]
  end

  defmessage PuppetStops do
    opcode "PUPPET_STOPS"

    content [
      :puppet_key,
      :position,
    ]
  end

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
    if MapSet.member?(puppets, puppet_key) do
      {:ok, beac} = Beacon.start(instance_key)
      beac |> Beacon.set_periodic_callback(300, &Lykan.System.Physics.puppet_moves(&1, puppet_key))
      |> Beacon.enable()

      dir = Body.get_direction(puppet_key)
      pos = Body.get_position(puppet_key)

      notify(&Lykan.Puppeteer.notify(&1, PuppetStarts.craft(puppet_key, dir, pos)))

      Map.put(state, puppet_key, beac)
    else
      state
    end
  end

  cast puppet_moves(puppet_key :: Lkn.Core.Puppet.k) do
    if MapSet.member?(puppets, puppet_key) do
      dir = Body.get_direction(puppet_key)
      pos = Body.get_position(puppet_key)
      bound = World.boundaries(map_key)

      pos = moves(pos, bound, dir)

      Body.set_position(puppet_key, pos)

      notify(&Lykan.Puppeteer.notify(&1, PuppetMoves.craft(puppet_key, dir, pos)))
    end

    state
  end

  cast puppet_stops_moving(puppet_key :: Lkn.Core.Puppet.k) do
    if MapSet.member?(puppets, puppet_key) do
      case Map.fetch(state, puppet_key) do
        {:ok, beac} ->
          Beacon.cancel(beac)

          pos = Body.get_position(puppet_key)
          dir = Body.get_direction(puppet_key)

          notify(&Lykan.Puppeteer.notify(&1, PuppetStops.craft(puppet_key, pos)))

          Map.delete(state, puppet_key)
        _ ->
          state
      end
    else
      state
    end
  end

  defp moves(pos, {_max_x, max_y}, :up) do
    Position.new(pos.x, min(max_y, pos.y + 5))
  end
  defp moves(pos, {_max_x, _max_y}, :down) do
    Position.new(pos.x, max(0, pos.y - 5))
  end
  defp moves(pos, {_max_x, _max_y}, :left) do
    Position.new(max(0, pos.x - 5), pos.y)
  end
  defp moves(pos, {max_x, _max_y}, :right) do
    Position.new(min(max_x, pos.x + 5), pos.y)
  end
end
