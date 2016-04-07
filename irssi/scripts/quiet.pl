#  Copyright (c) Christoph Berg <cb@df7cb.de>
#  All rights reserved.
#  
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  1. Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#  3. Neither the name of the University nor the names of its contributors
#     may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
#  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
#  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
#  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
#  SUCH DAMAGE.

#  This script adds support for +q (quiet user) channel modes to irssi.

use strict;
use Irssi;
use Irssi::Irc;

# :helium.oftc.net 344 Tauon #test test!*@* cryogen!stu@oftc.net 1164222156
# :helium.oftc.net 345 Tauon #test :End of Channel Quiet List

sub event_quiet_list
{
	my ($server, $data, $srvname) = @_;
	my ($target, $channel, $mask, $by, $time) = split(/\s+/, $data);
	$time = time() - $time if $time;
	$server->window_find_item($channel)->printformat(MSGLEVEL_CRAP,
		$by ? "quietlist_long" : "quietlist", $channel, $mask, $by, $time);
}

sub event_quiet_list_end
{
	my ($server, $data, $srvname) = @_;
	my ($target, $channel, $text) = split(/\s+/, $data, 3);
	$text =~ s/^://;
	$server->window_find_item($channel)->print($text, MSGLEVEL_CRAP);
}

sub do_quiet
{
	my ($data, $server, $witem, $quiet) = @_;
	my $support = $server->isupport("CHANMODES");
	if ($support !~ /q/) {
		Irssi::print("This server does not support channel mode +q");
		return;
	}
	if (!$witem or $witem->{type} ne "CHANNEL") {
		Irssi::print("Not joined to any channel");
		return;
	}
	my @data = split /\s+/, $data;
	my $mode = @data > 0 ? ($quiet ? "+" : "-") . ("q" x (@data)) . " @data" : "+q";
	$witem->command("mode $witem->{name} $mode");
}

sub quiet { do_quiet(@_, 1); }
sub unquiet { do_quiet(@_, 0); }

Irssi::theme_register([
	"quietlist" => '{channel $0}: ban quiet {ban $1}',
	"quietlist_long" => '{channel $0}: ban quiet {ban $1} {comment by {nick $2}, $3 secs ago}',
]);

Irssi::signal_add("event 344", "event_quiet_list");
Irssi::signal_add("event 345", "event_quiet_list_end");

Irssi::command_bind('quiet', 'quiet');
Irssi::command_bind('unquiet', 'unquiet');
