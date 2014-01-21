defmodule ElixirWebtest.SubscriberStore do
  use GenServer.Behaviour

  def start_link( subscribers ) do
    :gen_server.start_link({:local, :subscriber_store}, __MODULE__, subscribers, [])
  end

  def init( subscribers ) do
    { :ok, subscribers }
  end

  # add a subscriber
  def handle_cast( { :add, new_subscriber }, subscribers ) do
    IO.puts "adding newsubscriber: #{ inspect new_subscriber }"
    { :noreply, add_subscriber(subscribers, new_subscriber) }
  end

  # remove a subscriber
  def handle_cast( { :del, subscriber }, subscribers ) do
    { :noreply, remove_subscriber( subscribers, subscriber ) }
  end

  def handle_cast( { :broadcast, event }, subscribers ) do
    IO.puts "broadcast: #{ inspect event }"

    Enum.each subscribers, fn(sub) ->
      send( sub, event )
    end

    { :noreply, subscribers }
  end

  defp remove_subscriber( subscribers, subscriber ) do
    IO.puts "removing subscriber"
    List.delete subscribers, subscriber
  end

  defp add_subscriber( subscribers, new_subscriber ) do
    if Enum.any?( subscribers, fn(x) -> x == new_subscriber end ) do
      subscribers
    else
      [new_subscriber|subscribers]
    end
  end

end
