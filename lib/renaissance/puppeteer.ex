use Lkn.Prelude
import Lkn.Core.Puppeteer, only: [defpuppeteer: 2]

defpuppeteer Renaissance.Puppeteer do
  @cast puppet_color(puppet :: Lkn.Core.Puppet.k, c :: String.t)
end
