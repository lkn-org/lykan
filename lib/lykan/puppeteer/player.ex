use Lkn.Prelude

defmodule Lykan.Puppeteer.Player do
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
    Socket.Web.send!(
      state.socket,
      {:text, "[Puppet(#{inspect puppet_key})] enters, digest is #{inspect digest}"}
    )
    state
  end

  def puppet_leave(state, _instance_key, puppet_key) do
    Socket.Web.send!(
      state.socket,
      {:text, "[Puppet(#{inspect puppet_key})] leaves"}
    )
    state
  end

  def notify(_key, msg, _instance_key, state) do
    Socket.Web.send! state.socket, {:text, Lykan.Message.encode!(msg) }
    state
  end
  def puppet_color(_key, puppet, c, _instance_key, state) do
    state
  end

  def instance_digest(state, instance_key, map_key, map, puppets) do
    Socket.Web.send! state.socket, {
      :text,
      "[Instance(#{inspect instance_key})] for [Map(#{inspect map_key})]: #{inspect map} puppets: #{inspect puppets}"
    }
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
