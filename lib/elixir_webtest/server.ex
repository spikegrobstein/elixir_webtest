defmodule ElixirWebtest.Server do
  use GenServer.Behaviour

  def start_link(users) do
    :gen_server.start_link({:local, :userstore}, __MODULE__, users, [])
  end

  def init(users) do
    {:ok, users}
  end

  def handle_call( :list_users, _from, users ) do
    { :reply, users, users }
  end

  def handle_cast( { :add_user, new_user }, users ) do
    { :noreply, add_user( users, new_user ) }
  end

  def handle_cast( { :del_user, user }, users ) do
    { :noreply, del_user( users, user ) }
  end

  defp add_user( users, new_user ) do
    if Enum.any?( users, fn(x) -> x == new_user end ) do
      users
    else
      :gen_server.cast :subscriber_store, { :broadcast, { :add, new_user } }
      [new_user|users]
    end
  end

  defp del_user( users, user ) do
    if Enum.any?( users, fn(x) -> x == user end ) do
      :gen_server.cast :subscriber_store, { :broadcast, { :del, user } }
      List.delete( users, user )
    else
      users
    end
  end

end
