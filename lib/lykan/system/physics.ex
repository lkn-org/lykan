use Lkn.Prelude

import Lkn.Core.System, only: [defsystem: 2]
import Lkn.Core.Component, only: [defcomponent: 2]

alias Lkn.Physics.Geometry.Box
alias Lkn.Physics.Geometry.Vector
alias Lkn.Physics

import Lykan.Message, only: [defmessage: 2]

defsystem Lykan.System.Physics do
  @type direction :: :up | :down | :right | :left
  @type entry_point :: any

  defmodule Teleporter do
    defstruct [
      :at,
      :to,
    ]

    @type t :: %Teleporter{
      at: Physics.Body.t,
      to: {Lkn.Core.Map.t, Lykan.System.Physics.entry_point},
    }

    def new(at, to) do
      %Teleporter{
        at: at,
        to: to,
      }
    end
  end


  defcomponent Body do
    @system Lykan.System.Physics

    @call get_direction() :: Lykan.System.Physics.direction
    @cast set_direction(dir :: Lykan.System.Physics.direction)

    @call get_position() :: Vector.t
    @cast set_position(pos :: Vector.t)

    @call get_box() :: Box.t
  end

  defcomponent World do
    @system Lykan.System.Physics

    @call boundaries() :: {non_neg_integer, non_neg_integer}
    @call get_teleporters() :: [Teleporter]
  end

  @map World
  @puppet Body

  #############################################################################

  defmessage PuppetStarts do
    opcode "PUPPET_STARTS"

    content [
      :puppet_key,
    ]
  end

  defmessage PuppetMoves do
    opcode "PUPPET_MOVES"

    content [
      :puppet_key,
      :position,
      :colliders,
    ]
  end

  defmessage PuppetStops do
    opcode "PUPPET_STOPS"

    content [
      :puppet_key,
    ]
  end

  defmessage PuppetHitsTeleport do
    opcode "PUPPET_TELEPORTS"

    content [
      :puppet_key,
      :map,
      :entry_point,
    ]
  end

  #############################################################################
  defmodule State do
    @moduledoc false

    defstruct [
      :world,
      :moving,
      :teleporters,
    ]

    def new(map_key, w, h) do
      tels = World.get_teleporters(map_key)

      world = Enum.reduce(tels, Physics.World.new(w, h), fn (tel, world) ->
        Physics.World.add(world, {:teleporters, tel.to}, tel.at)
      end)

      %State{
        world: world,
        moving: Map.new(),
      }
    end

    def add(state, key, body) do
      %State{state|
             world: Physics.World.add(state.world, key, body)
      }
    end

    def remove(state, key) do
      state = stops_moving(state, key)

      %State{state|
        world: Physics.World.remove(state.world, key),
      }
    end

    def move?(state, key) do
      Map.has_key?(state.moving, key)
    end

    def starts_moving(state, key, beac) do
      %State{state|moving: Map.put(state.moving, key, beac)}
    end

    def stops_moving(state, key) do
      {mbeac, mov} = Map.pop(state.moving, key)

      case mbeac do
        nil -> nil
        beac -> Beacon.cancel(beac)
      end

      %State{state|moving: mov}
    end
  end

  #############################################################################
  def init_state(instance_key, map_key) do
    {w, h} = World.boundaries(map_key)

    State.new(map_key, w, h)
  end

  def puppet_enter(state, _instance_key, _map_key, _puppets, puppet_key) do
    vec = fn -> Body.get_position(puppet_key) end
    bod = Body.get_box(puppet_key)

    cast_return(state: State.add(state, puppet_key, Physics.Body.new(vec, bod, false)))
  end

  def puppet_leave(state, _instance_key, _map_key, _puppets, puppet_key) do
    cast_return(state: State.remove(state, puppet_key))
  end

  def handle_collision(state, puppet_key, collisions) do
    Enum.reduce(collisions, [], fn (key, untreated) ->
      case key do
        :right_bound -> untreated
        :left_bound -> untreated
        :top_bound -> untreated
        :bottom_bound -> untreated
        {:teleporters, {map, ep}} ->
          notify(&Lykan.Puppeteer.notify(&1, PuppetHitsTeleport.craft(puppet_key, map, ep)))
          untreated
        x -> [x|untreated]
      end
    end)
  end

  #############################################################################
  cast puppet_changes_dir(puppet_key :: Lkn.Core.Puppet.k, dir :: direction) do
    Body.set_direction(puppet_key, dir)

    cast_return()
  end

  cast puppet_starts_moving(puppet_key :: Lkn.Core.Puppet.k) do
    if MapSet.member?(puppets, puppet_key) && !State.move?(state, puppet_key) do
      {:ok, beac} = Beacon.start(instance_key)
      beac |> Beacon.set_periodic_callback(150, &Lykan.System.Physics.puppet_moves(&1, puppet_key))
           |> Beacon.enable()

      notify(&Lykan.Puppeteer.notify(&1, PuppetStarts.craft(puppet_key)))

      cast_return(state: State.starts_moving(state, puppet_key, beac))
    else
      cast_return()
    end
  end

  cast puppet_moves(puppet_key :: Lkn.Core.Puppet.k) do
    if MapSet.member?(puppets, puppet_key) do
      dir = Body.get_direction(puppet_key)

      {vec, col} =
        Physics.World.move(state.world, puppet_key, case dir do
                                                      :up -> Vector.new(0, 5)
                                                      :down -> Vector.new(0, -5)
                                                      :right -> Vector.new(5, 0)
                                                      :left -> Vector.new(-5, 0)
                                                    end)

      col = handle_collision(state, puppet_key, col)

      new_pos = Vector.add(Body.get_position(puppet_key), vec)
      Body.set_position(puppet_key, new_pos)

      notify(&Lykan.Puppeteer.notify(&1, PuppetMoves.craft(puppet_key, new_pos, col)))
      cast_return()
    else
      cast_return()
    end
  end

  cast puppet_stops_moving(puppet_key :: Lkn.Core.Puppet.k) do
    if MapSet.member?(puppets, puppet_key) do
      state = State.stops_moving(state, puppet_key)

      notify(&Lykan.Puppeteer.notify(&1, PuppetStops.craft(puppet_key)))

      cast_return(state: state)
    else
      cast_return()
    end
  end
end
