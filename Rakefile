RACKET_PATH = "/opt/homebrew-cask/Caskroom/racket/6.1.1/Racket\ v6.1.1/bin/racket"

def sicp_requires
  Dir['sicp/*.rkt'].map { |file|
    ["-e", %Q{(require "#{file}")}]
  }.inject :+
end

def sicp_invocation extras
  [RACKET_PATH] + sicp_requires + extras
end

def sicp_shell_invocation
  sicp_invocation %W{-l xrepl -i }
end
def sicp_test_invocation
  exercises = %W{
   1.1
   1.2
   1.3
   1.7
   1.8
   1.10
   1.11
  }

  sicp_invocation %W{ -l rackunit -l rackunit/text-ui } +
    ["-e", exercises.map {|ex| "(run-tests ex-#{ex}-tests)"}.join(" ")]
end

def sys(*args)
  puts args.map{|it| "'#{it}'" }.join " "
  system *args
end

task :build do
  sys *sicp_test_invocation
end

namespace :sicp do
  task :shell do
    sys *sicp_shell_invocation
  end
end

task :default => :build
