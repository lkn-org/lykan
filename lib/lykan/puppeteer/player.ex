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

  def puppet_enter(state, _instance_key, _puppet_key, _digest) do
    state
  end

  def puppet_leave(state, _instance_key, _puppet_key) do
    state
  end

  def notify(_key, notification, _instance_key, state) do
    Socket.Web.send! state.socket, {:text, inspect(notification) }
    state
  end
  def puppet_color(_key, puppet, c, _instance_key, state) do
    state
  end
end
