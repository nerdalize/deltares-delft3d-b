#! /usr/bin/ruby

#-------------------------------------------------------------------------------
#   This script gathers all of the libraries needed by the programs and/or
#   shared libraries specified on the command line and echoes them.
#
#   Irv.ELshoff@Deltares.NL
#   29 jun 12
#-------------------------------------------------------------------------------


$libraries = {}

def gather (pathname)
    if ! $libraries[pathname]
        $libraries[pathname] = true
        IO.popen("ldd #{pathname}").each do |line|
            a = line.chomp.split
            if a[1] == '=>' && a[2] =~ /^(\/|\.\/)/
                $queue << a[2]
            elsif a[0] =~ /^\//
                $queue << a[0]
            end
        end
    end
end

$queue = ARGV + []
$queue.each do |pathname|
    gather pathname
end

($libraries.keys - ARGV).sort.each do |pathname|
    puts pathname
end
