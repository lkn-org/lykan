use Lkn.Prelude

import Lkn.Core.System, only: [defsystem: 2]
import Lkn.Core.Component, only: [defcomponent: 2]

import Lykan.Message, only: [defmessage: 2]

alias Lkn.Physics.Geometry.Box
alias Lkn.Physics.Geometry.Vector
alias Lkn.Physics

defsystem Lykan.System.Combat do
  defmodule Component do
    defcomponent Map do
      @system Lykan.System.Combat
    end

    defcomponent Puppet do
      @system Lykan.System.Combat

      @call attack_zone() :: {Box.t, Vector.t}
    end
  end

  @map Component.Map
  @puppet Component.Puppet

  defmodule State do
    defstruct [
      :world,
      :attackers,
    ]

    def new(map_key) do
      %State{
        world: Physics.World.new(),
        attackers: Map.new(),
      }
    end

    def new_target(state, puppet_key) do
      vec = fn -> Lykan.System.Physics.Body.get_position(puppet_key) end
      bod = Lykan.System.Physics.Body.get_box(puppet_key)

      %State{state|world: Physics.World.add(state.world,
                                            puppet_key,
                                            Physics.Body.new(vec, bod, false))}
    end

    def remove_target(state, puppet_key) do
      state = remove_attacker(state, puppet_key)

      %State{state|world: Physics.World.remove(state.world, puppet_key)}
    end

    def new_attacker(state, instance_key, puppet_key) do
      {:ok, beac} = Beacon.start(instance_key)
      beac |> Beacon.set_periodic_callback(300, &Lykan.System.Combat.puppet_attacks(&1, puppet_key))
           |> Beacon.enable()
      %State{state|attackers: Map.put(state.attackers, puppet_key, beac)}
    end

    def remove_attacker(state, puppet_key) do
      {mbeac, attackers} = Map.pop(state.attackers, puppet_key)

      case mbeac do
        nil -> nil
        beac -> Beacon.cancel(beac)
      end

      %State{state|attackers: attackers}
    end
  end

  @doc false
  def init_state(_instance_key, map_key) do
    State.new(map_key)
  end

  @doc false
  def puppet_enter(state, _instance_key, map_key, _puppets, puppet_key, _opts) do
    cast_return(state: State.new_target(state, puppet_key))
  end

  @doc false
  def puppet_leave(state, _instance_key, _map_key, _puppets, puppet_key) do
    cast_return(state: State.remove_target(state, puppet_key))
  end

  cast puppet_start_attacking(puppet_key :: Lkn.Core.Puppet.k) do
    if !Map.has_key?(state, puppet_key) do
      notify(&Lykan.Puppeteer.notify(&1, PuppetStartsAttacking.craft(puppet_key)))

      cast_return(state: State.new_attacker(state, instance_key, puppet_key))
    else
      cast_return()
    end
  end

  cast puppet_stop_attacking(puppet_key :: Lkn.Core.Puppet.k) do
    if Map.has_key?(state.attackers, puppet_key) do
      notify(&Lykan.Puppeteer.notify(&1, PuppetStopsAttacking.craft(puppet_key)))

      cast_return(state: State.remove_attacker(state, puppet_key))
    else
      cast_return()
    end
  end

  cast puppet_attacks(puppet_key :: Lkn.Core.Puppet.k) do
    notify(&Lykan.Puppeteer.notify(&1, PuppetAttacks.craft(puppet_key)))

    {box, vec} = Component.Puppet.attack_zone(puppet_key)

    victims = Physics.World.collide_with(state.world, Physics.Body.new(vec, box))
              |> Enum.filter(&(&1 != puppet_key))

    # TODO: picking the first element of the list does not seems fair?
    case victims do
      [victim|_] -> notify(&Lykan.Puppeteer.notify(&1, PuppetHurted.craft(victim)))
      _ -> :ok
    end

    cast_return()
  end

  defmessage PuppetHurted do
    opcode "PUPPET_HURTED"

    content [
      :puppet_key,
    ]
  end

  defmessage PuppetStartsAttacking do
    opcode "PUPPET_STARTS_ATTACKING"

    content [
      :puppet_key,
    ]
  end

  defmessage PuppetAttacks do
    opcode "PUPPET_ATTACKS"

    content [
      :puppet_key,
    ]
  end

  defmessage PuppetStopsAttacking do
    opcode "PUPPET_STOPS_ATTACKING"

    content [
      :puppet_key,
    ]
  end
end
