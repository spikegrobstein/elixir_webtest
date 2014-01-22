defmodule ElixirWebtest.UserStore do
  use GenServer.Behaviour

  def start_link( users ) do
    :gen_server.start_link({:local, :user_store}, __MODULE__, users, [])
  end

  def init( users ) do
    {:ok, users}
  end

  @doc """
    return the list of users
  """
  def handle_call( :list_users, _from, users ) do
    { :reply, users, users }
  end

  @doc """
    add the given user
  """
  def handle_cast( { :add_user, new_user }, users ) do
    { :noreply, add_user( users, new_user ) }
  end

  @doc """
    delete the given user
  """
  def handle_cast( { :del_user, user }, users ) do
    { :noreply, del_user( users, user ) }
  end

  @doc """
    Add the user to the user list.

    First check to see if new_user is currently in the users list,
    if so, return users unchanged
    otherwise, signal the subscriber_store that new_user was added
    and add new_user to the users

    returns the current state of users
  """
  defp add_user( users, new_user ) do
    if Enum.any?( users, fn(x) -> x == new_user end ) do
      users
    else
      :gen_server.cast :subscriber_store, { :broadcast, { :add, new_user } }
      [new_user|users]
    end
  end

  @doc """
    remove the given user from the user list

    check if user is in users
    if so, broadcast that its being removed, then remove it
    if not, just return users

    returns the current state of users
  """
  defp del_user( users, user ) do
    if Enum.any?( users, fn(x) -> x == user end ) do
      :gen_server.cast :subscriber_store, { :broadcast, { :del, user } }
      List.delete( users, user )
    else
      users
    end
  end

end
