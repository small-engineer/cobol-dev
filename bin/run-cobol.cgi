#!/usr/bin/env perl
use strict;
use warnings;
use IO::Socket::INET;
use JSON::PP;
use POSIX qw(strftime);

my $host    = $ENV{COBOL_SERVER_HOST};
my $port    = $ENV{COBOL_SERVER_PORT};
my $timeout = 3;

my ($status, $payload);
my $sock = IO::Socket::INET->new(
    PeerAddr => $host,
    PeerPort => $port,
    Proto    => 'tcp',
    Timeout  => $timeout,
);

if ($sock) {
    my $resp = '';
    $sock->recv($resp, 4096);
    close $sock;
    $status  = 'ok';
    $payload = $resp // '';
} else {
    $status  = 'error';
    $payload = "connect failed: $!";
}

print "Content-Type: application/json; charset=UTF-8\r\n\r\n";
print encode_json {
    status   => $status,
    cobol    => $payload,
    datetime => strftime('%Y-%m-%dT%H:%M:%S', localtime),
};
exit;
