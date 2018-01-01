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
      :puppets,
      :instance,
    ]

    def new(socket) do
      %__MODULE__{
        socket: socket,
        puppets: [],
      }
    end

    def add_puppet(state, instance_key, puppet) do
      case instance_key do
        Option.some(instance) ->
          Lkn.Core.Instance.register_puppet(instance, puppet)
        _ -> :ok
      end

      %Info{state|puppets: [puppet | state.puppets]}
    end
  end

  use Lykan.Puppeteer do
    cast assign_puppet(puppet_key :: Lkn.Core.Puppet.k) do
      Info.add_puppet(state, instance_key, puppet_key)
    end
  end

  def init_state(socket) do
    {:ok, Info.new(socket)}
  end

  def start_link(puppeteer_key, socket) do
    Lkn.Core.Puppeteer.start_link(__MODULE__, puppeteer_key, socket)
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
    Enum.map(state.puppets, fn puppet ->
      Lkn.Core.Instance.unregister_puppet(instance_key, puppet)
    end)

    Lkn.Core.Instance.unregister_puppeteer(instance_key, key)

    destroy(key, state, Option.nothing(), reason)
  end
  def destroy(key, state, _none, _reason) do
    # we kill our puppets
    Enum.map(state.puppets, fn puppet ->
      Lkn.Core.Entity.stop(puppet)
    end)
  end
end
