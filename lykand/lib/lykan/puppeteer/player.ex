use Lkn.Prelude

import Lykan.Message, only: [defmessage: 2]

alias Lykan.Commands, as: C
alias Lykan.System.Physics.PuppetHitsTeleport

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
  end

  defmessage LeaveMap do
    opcode "LEAVE_MAP"
    content []
  end

  defmessage PuppetLeave do
    opcode "PUPPET_LEAVES"

    content [
      :puppet_key
    ]
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
        Option.some(instance_key) -> consume_cmd(Lykan.Command.decode(cmd), state, instance_key)
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

  defp consume_cmd(cmd = %C.SetDirection{}, state, instance_key) do
    Lykan.System.Physics.puppet_changes_dir(instance_key, state.puppet, cmd.angle)
    cast_return()
  end

  defp consume_cmd(cmd = %C.LookAt{}, _state, _instance_key) do
    IO.puts cmd.angle
    cast_return()
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
    Lykan.System.Physics.puppet_changes_dir(instance_key, state.puppet, Math.pi)

    cast_return()
  end

  defp consume_cmd("ATTACK", state, instance_key) do
    Lykan.System.Combat.puppet_start_attacking(instance_key, state.puppet)

    cast_return()
  end

  defp consume_cmd("STOP_ATTACK", state, instance_key) do
    Lykan.System.Combat.puppet_stop_attacking(instance_key, state.puppet)

    cast_return()
  end

  defp consume_cmd(_cmd, _state, _instance_key) do
    # unknown command, we bail and do nothing for now

    cast_return()
  end

  def leave_instance(state, _instance_key) do
    cast_return()
  end

  def puppet_enter(state, _instance_key, puppet_key, digest) do
    Lykan.Message.send(state.socket, PuppetEnter.craft(puppet_key, digest))

    cast_return()
  end

  def puppet_leave(state, _instance_key, puppet_key) do
    Lykan.Message.send(state.socket, PuppetLeave.craft(puppet_key))

    cast_return()
  end

  def notify(key, msg = %PuppetHitsTeleport{}, Option.some(instance_key), state) do
    if msg.puppet_key == state.puppet do
      # leaving the current instance
      Lkn.Core.Instance.unregister_puppet(instance_key, state.puppet)
      Lkn.Core.Instance.unregister_puppeteer(instance_key, key)

      # notify the client
      Lykan.Message.send(state.socket, LeaveMap.craft())

      # register to a new instance
      instance_key = Lkn.Core.Pool.register_puppeteer(msg.map, key, __MODULE__)

      # register our pupppet
      opts = %{
        Lykan.System.Physics => [entry_point: msg.entry_point],
      }
      Lkn.Core.Instance.register_puppet(instance_key, state.puppet, opts)

      cast_return(instance: Option.some(instance_key), map_key: msg.map)
    else
      cast_return()
    end
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

  def accept(client, default_map_key) do
    Socket.Web.accept!(client)

    # spawn a character for this player
    chara_key = UUID.uuid4()
    Lykan.Character.spawn(chara_key)

    # spawn a player puppeteer and find an instance
    puppeteer_key = UUID.uuid4()
    Lykan.Puppeteer.Player.start_link(puppeteer_key, client, chara_key)
    Lykan.Puppeteer.Player.goto(puppeteer_key, default_map_key)

    recv(puppeteer_key, client)
  end

  defp recv(puppeteer_key, client) do
    case Socket.Web.recv(client) do
      {:ok, {:text, msg}} ->
        Lykan.Puppeteer.Player.inject(puppeteer_key, msg)

        recv(puppeteer_key, client)
      _ ->
        Lkn.Core.Puppeteer.stop(puppeteer_key)
    end
  end
end
