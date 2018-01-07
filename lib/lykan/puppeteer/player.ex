use Lkn.Prelude

import Lykan.Message, only: [defmessage: 2]

alias Lykan.System.Physics.PuppetHitsTeleport
alias Lkn.Physics.Geometry.Vector

defmodule Lykan.Puppeteer.Player do
  defmessage AttributePuppet do
    opcode "ATTRIBUTE_PUPPET"
    content [
      :puppet_key,
    ]
  end
  defmessage PuppetEnter do
    opcode "PUPPET_ENTERS"
    content [
      :puppet_key,
      :digest,
    ]

    def new(key, digest) do
      %PuppetEnter{
        puppet_key: key,
        digest: digest,
      }
    end
  end

  defmessage PuppetLeave do
    opcode "PUPPET_LEAVES"

    content [
      :puppet_key
    ]

    def new(key) do
      %PuppetLeave{
        puppet_key: key
      }
    end
  end

  defmessage InstanceDigest do
    opcode "INSTANCE_DIGEST"

    content [
      :map,
      :puppets,
    ]

    def craft(map_key, map, puppets) do
      craft(%{:map_key => map_key, :digest => map}, puppets)
    end
  end

  defmodule State do
    @moduledoc false

    defstruct [
      :socket,
      :puppet,
    ]

    def new(socket, main_puppet) do
      %__MODULE__{
        socket: socket,
        puppet: main_puppet,
      }
    end
  end

  use Lykan.Puppeteer do
    cast goto(map_key :: Lkn.Core.Map.k) do
      Option.map(instance_key, fn key ->
        Lkn.Core.Instance.unregister_puppet(key, state.puppet)
        Lkn.Core.Instance.unregister_puppeteer(key, puppeteer_key)
      end)

      instance_key = Lkn.Core.Pool.register_puppeteer(map_key, puppeteer_key, Lykan.Puppeteer.Player)

      cast_return(instance: Option.some(instance_key), map_key: map_key)
    end

    cast inject(cmd :: any) do
      case instance_key do
        Option.some(instance_key) -> consume_cmd(cmd, state, instance_key)
        Option.nothing() -> cast_return()
      end
    end
  end

  def init_state(socket: socket, main: puppet) do
    Lykan.Message.send(socket, AttributePuppet.craft(puppet))

    {:ok, State.new(socket, puppet)}
  end

  def start_link(puppeteer_key, socket, main) do
    Lkn.Core.Puppeteer.start_link(__MODULE__, puppeteer_key, socket: socket, main: main)
  end

  defp consume_cmd("MOVE", state, instance_key) do
    Lykan.System.Physics.puppet_starts_moving(instance_key, state.puppet)

    cast_return()
  end

  defp consume_cmd("STOP", state, instance_key) do
    Lykan.System.Physics.puppet_stops_moving(instance_key, state.puppet)

    cast_return()
  end

  defp consume_cmd("UP", state, instance_key) do
    Lykan.System.Physics.puppet_changes_dir(instance_key, state.puppet, :up)

    cast_return()
  end

  defp consume_cmd("DOWN", state, instance_key) do
    Lykan.System.Physics.puppet_changes_dir(instance_key, state.puppet, :down)

    cast_return()
  end

  defp consume_cmd("RIGHT", state, instance_key) do
    Lykan.System.Physics.puppet_changes_dir(instance_key, state.puppet, :right)

    cast_return()
  end

  defp consume_cmd("LEFT", state, instance_key) do
    Lykan.System.Physics.puppet_changes_dir(instance_key, state.puppet, :left)

    cast_return()
  end

  def leave_instance(state, _instance_key) do
    cast_return()
  end

  def puppet_enter(state, _instance_key, puppet_key, digest) do
    Lykan.Message.send(state.socket, PuppetEnter.new(puppet_key, digest))

    cast_return()
  end

  def puppet_leave(state, _instance_key, puppet_key) do
    Lykan.Message.send(state.socket, PuppetLeave.new(puppet_key))

    cast_return()
  end

  def notify(key, msg = %PuppetHitsTeleport{}, Option.some(instance_key), state) do
    Lkn.Core.Instance.unregister_puppet(instance_key, state.puppet)
    Lkn.Core.Instance.unregister_puppeteer(instance_key, key)

    instance_key = Lkn.Core.Pool.register_puppeteer(msg.map, key, __MODULE__)

    Lykan.System.Physics.Body.set_position(state.puppet, Vector.new(0, 10))


    Lkn.Core.Instance.register_puppet(instance_key, state.puppet)

    cast_return(instance: Option.some(instance_key), map_key: msg.map)
  end
  def notify(_key, msg, _instance_key, state) do
    Lykan.Message.send(state.socket, msg)

    cast_return()
  end

  def instance_digest(state, instance_key, map_key, map, puppets) do
    Lykan.Message.send(state.socket, InstanceDigest.craft(map_key, map, puppets))

    Lkn.Core.Instance.register_puppet(instance_key, state.puppet)

    cast_return()
  end

  def destroy(key, state, Option.some(instance_key), reason) do
    # we leave the instance in a clean way
    Lykan.System.Physics.puppet_stops_moving(instance_key, state.puppet)
    Lkn.Core.Instance.unregister_puppet(instance_key, state.puppet)

    Lkn.Core.Instance.unregister_puppeteer(instance_key, key)

    destroy(key, state, Option.nothing(), reason)
  end
  def destroy(_key, state, _none, _reason) do
    # we kill our puppet
    Lkn.Core.Entity.stop(state.puppet)
  end
end
