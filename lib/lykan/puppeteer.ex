use Lkn.Prelude
import Lkn.Core.Puppeteer, only: [defpuppeteer: 2]

defpuppeteer Lykan.Puppeteer do
  @cast notify(notification :: term)
end
