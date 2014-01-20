defmodule ElixirWebtest.SubscriberStore do
  use GenServer.Behaviour

  def start_link( subscribers ) do
    :gen_server.start_link({:local, :subscriber_store}, __MODULE__, subscribers, [])
  end

  def init( subscribers ) do
    IO.puts "starting subscriberstore #{ inspect subscribers }"
    { :ok, subscribers }
  end

  def handle_cast( { :add, new_subscriber }, subscribers ) do
    IO.puts "adding newsubscriber: #{ inspect new_subscriber }"
    { :noreply, [ new_subscriber | subscribers ] }
  end

  def handle_cast( { :broadcast, event }, subscribers ) do
    IO.puts "broadcast: #{ inspect event }"

    Enum.each subscribers, fn(sub) ->
      send( sub, event )
    end

    { :noreply, subscribers }
  end
end
