class API < Sinatra::Base
  def initialize(ledger: Ledger.new)
    @ledger = ledger
    super() # rest of init from Sinatra
  end
end

# Later, callers do this
app = API.new(ledger: Ledger.new)
