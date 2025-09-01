# NAME

[queue](../lib/queue.pm)

# SYNOPSIS

    # Create a queue object with 4 working threads
    my $queue = 'queue'->new (threads => 4, verbose => 1);

    for my $file (@lst)
      {
        # Invoke process method of 'main' on argument list
        $queue->append (['main', 'process', [file => $file, %opts]]); 
      }
    
    # Wait for all tasks to complete
    $queue->flush ();

# DESCRIPTION

This class wraps the `Thread::Queue` class and manages a gang of threads. The user
can then submit tasks to the queue; tasks may either be:

- A package name followed by a method name, followed by a reference on a list containing
arguments.
- A list of character strings; this will be executed with `system`.

# SEE ALSO

threads, Thread::Queue

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
