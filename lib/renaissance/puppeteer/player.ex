use Lkn.Prelude

defmodule Renaissance.Puppeteer.Player do
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
        instance: Option.nothing(),
      }
    end

    def set_instance(state, instance) do
      Enum.map(state.puppets, &(Lkn.Core.Instance.register_puppet(instance, &1)))

      %Info{state|instance: Option.some(instance)}
    end

    def add_puppet(state, puppet) do
      case state.instance do
        Option.some(instance) ->
          Lkn.Core.Instance.register_puppet(instance, puppet)
        _ -> :ok
      end

      %Info{state|puppets: [puppet | state.puppets]}
    end
  end

  use Renaissance.Puppeteer do
    cast goto(map_key :: Lkn.Core.Map.k) do
      instance_key = Lkn.Core.Pool.register_puppeteer(map_key, key, __MODULE__)

      Info.set_instance(state, instance_key)
    end

    cast kill() do
      case state.instance do
        Option.some(instance) ->
          Enum.map(state.puppets, &(Lkn.Core.Instance.unregister_puppet(instance, &1)))
          Lkn.Core.Instance.unregister_puppeteer(instance, key)
        _ -> :ok
      end
      state
    end

    cast assign_puppet(puppet_key :: Lkn.Core.Puppet.k) do
      Info.add_puppet(state, puppet_key)
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

  def puppet_color(_key, puppet, c, state) do
    Socket.Web.send! state.socket, {:text, "[Puppet(#{inspect puppet})] new color is #{c}"}
    state
  end
end
