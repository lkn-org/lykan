use Lkn.Prelude

import Lykan.Message, only: [defmessage: 2]

defmodule Lykan.Puppeteer.Player do
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

  defmodule Info do
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
    cast inject(cmd :: any) do
      case instance_key do
        Option.some(instance_key) -> consume_cmd(cmd, state, instance_key)
        Option.nothing() -> state
      end
    end
  end

  def init_state(socket: socket, main: puppet) do
    {:ok, Info.new(socket, puppet)}
  end

  def start_link(puppeteer_key, socket, main) do
    Lkn.Core.Puppeteer.start_link(__MODULE__, puppeteer_key, socket: socket, main: main)
  end

  defp consume_cmd("MOVE", state, instance_key) do
    Lykan.System.Physics.puppet_starts_moving(instance_key, state.puppet)
    state
  end

  defp consume_cmd("STOP", state, instance_key) do
    Lykan.System.Physics.puppet_stops_moving(instance_key, state.puppet)
    state
  end

  def leave_instance(state, _instance_key) do
    state
  end

  def puppet_enter(state, _instance_key, puppet_key, digest) do
    Lykan.Message.send(state.socket, PuppetEnter.new(puppet_key, digest))

    state
  end

  def puppet_leave(state, _instance_key, puppet_key) do
    Lykan.Message.send(state.socket, PuppetLeave.new(puppet_key))

    state
  end

  def notify(_key, msg, _instance_key, state) do
    Lykan.Message.send(state.socket, msg)

    state
  end
  def puppet_color(_key, puppet, c, _instance_key, state) do
    state
  end

  def instance_digest(state, instance_key, map_key, map, puppets) do
    Lykan.Message.send(state.socket, InstanceDigest.craft(map_key, map, puppets))

    Lkn.Core.Instance.register_puppet(instance_key, state.puppet)

    state
  end

  def destroy(key, state, Option.some(instance_key), reason) do
    # we leave the instance in a clean way
    Lykan.System.Physics.puppet_stops_moving(instance_key, state.puppet)
    Lkn.Core.Instance.unregister_puppet(instance_key, state.puppet)

    Lkn.Core.Instance.unregister_puppeteer(instance_key, key)

    destroy(key, state, Option.nothing(), reason)
  end
  def destroy(key, state, _none, _reason) do
    # we kill our puppet
    Lkn.Core.Entity.stop(state.puppet)
  end
end
