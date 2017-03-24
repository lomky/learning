class Racer

  # convenience method of access to client in console
  def self.mongo_client
    Mongoid::Clients.default
  end

  # convenience method of access to zips collection
  def self.collection
    self.mongo_client['racers']
  end

  def self.all(prototype={}, sort={:number=>1}, offset=0, limit=100)
    prototype=prototype.symbolize_keys.slice(:number, :first_name, :last_name, :gender, :group, :secs) if !prototype.nil?
    result=collection.find(prototype)
          .projection({_id:true, number:true, first_name:true, last_name:true, gender:true, group:true, secs:true})
          .sort(sort)
          .skip(offset)
    result=result.limit(limit) if !limit.nil?

    return result
  end
end

