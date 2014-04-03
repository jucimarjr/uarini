defmodule Pingpong do
  @moduledoc """
  MPI Benchmark [Pingpong] in elixir by Filipe Varjão <frgv@cin.ufpe.br>
  """

  def run(size, r) do
    data = generate_data(size)

    spawnStart = time_microseg()

    p1 = spawn(fn -> pingpong(data, self, r) end)
    p2 = spawn(fn -> pingpong(data, self, r) end)
    spawnEnd = time_microseg()
    timeStart = time_microseg()
    send(p1, {:init, self, p2})
    finalize(p1)
    timeEnd = time_microseg()
    totalTime = timeEnd - timeStart
    spawnTime = spawnEnd - spawnStart

    # PRINT RESULT
    IO.puts "bytes #{:erlang.size(data)} | repetitions #{r} | exec_time[µsec] #{totalTime} | MBytes/sec #{spawnTime} | spawn_time #{bandwidth_calc(data, totalTime)}"
  end

    def pingpong(_, pid, 0), do: send(pid ,{:finish, self})

  def pingpong(data, pid, r) do
    receive do
      {:init, pid, peer} ->
        send(peer, {self, data})
        pingpong(data, pid, r - 1)
      {peer, data} ->
        send(peer, {self, data})
        pingpong(data, pid, r - 1)
    end
  end

  def finalize(_p1) do
    receive do
      {:finish, _p1} ->
        :ok
    end
  end

  def bandwidth_calc(data, time) do
    megabytes = (:erlang.size(data) / :math.pow(2, 20))
    seconds = time * 1.0e-6
    megabytes / seconds
  end

  def generate_data(size), do: generate_data(size, [])

  def generate_data(0, bytes), do: iolist_to_binary(bytes)

  def generate_data(size, bytes), do: generate_data(size - 1, [1 | bytes])

  def time_microseg() do
    {ms, s, us} = :erlang.now()
    (ms * 1.0e+12) + (s * 1.0e+6) + us
  end

end
