class Racer
  attr_accessor :id, :number, :first_name, :last_name, :gender, :group, :secs

#  @id=doc[:_id].to_s
#  :_id=>BSON::ObjectId.from_string(@id))
#  # initialize from both a Mongo and Web hash
  def initialize(params={})
    #switch between both internal and external views of id
    @id=params[:_id].nil? ? params[:id].to_s : params[:_id].to_s
    @number=params[:number]
    @first_name=params[:first_name]
    @last_name=params[:last_name]
    @gender=params[:gender]
    @group=params[:group]
    @secs=params[:secs]
  end

  # convenience method of access to client in console
  def self.mongo_client
    Mongoid::Clients.default
  end

  # convenience method of access to zips collection
  def self.collection
    self.mongo_client['racers']
  end

  def self.all(prototype={}, sort={:number=>1}, offset=0, limit=nil)
    result=collection.find(prototype)
          .projection({_id:true, number:true, first_name:true, last_name:true, gender:true, group:true, secs:true})
          .sort(sort)
          .skip(offset)

    result=result.limit(limit) if !limit.nil?
    return result
  end

  def self.find id

    doc=collection.find(:_id=>id).first
    return doc.nil? ? nil : Zip.new(doc)
  end 


end

