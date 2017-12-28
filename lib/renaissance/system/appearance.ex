use Lkn.Prelude
import Lkn.Core.System, only: [defsystem: 2]
import Lkn.Core.Component, only: [defcomponent: 2]

defsystem Renaissance.System.Appearance do
  defcomponent Component do
    @system Renaissance.System.Appearance

    @call get_color() :: String.t
    @cast set_color(c :: String.t)
  end

  @map Component
  @puppet Component

  def init_state(_instance_key, _map_key) do
    :ok
  end

  def puppet_enter(_no_state, _instance_key, map_key, _puppets, key) do
    c = Component.get_color(map_key)
    Component.set_color(key, c)

    notify(&(Renaissance.Puppeteer.puppet_color(&1, key, c)))
    :ok
  end

  def puppet_leave(_no_state, _instance_key, _map_key, _puppets, _key) do
    :ok
  end

  cast change_puppet_color(key :: Lkn.Core.Puppet.k, c :: String.t) do
    Component.set_color(key, c)
    notify(&(Renaissance.Puppeteer.puppet_color(&1, key, c)))

    state
  end
end
