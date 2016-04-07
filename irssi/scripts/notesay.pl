use warnings;
use strict;
use utf8;

my @NOTES = qw/♩ ♪ ♫ ♬/;
my @MODIFIERS = qw/♭ ♮ ♯/;

sub random_choice {
    return @_[int rand scalar @_];
}

sub picknote {
    my $note = random_choice(@NOTES);
    if (int rand 4 == 0) {
        $note = random_choice(@MODIFIERS) . $note;
    }
    return $note;
}

sub notesay {
    my ($lyric) = @_;
    return picknote() . " $lyric " . picknote();
}

sub cmd_notesay {
    my ($data, $server, $witem) = @_;
    $data = notesay($data);
    $witem->command("say $data");
}

eval { Irssi::Core::is_static() };
if ($@) {
    use 5.010;
    binmode STDOUT, ":encoding(UTF-8)";
    say notesay($ARGV[0]);
} else {
    require Irssi;
    Irssi->import;
    Irssi::command_bind('notesay', 'cmd_notesay');
}
