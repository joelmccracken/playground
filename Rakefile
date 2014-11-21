
def sicp_requires
  Dir['sicp/*.rkt'].map { |file|
    ["-e", %Q{(require "#{file}")}]
  }.inject :+
end

def sicp_invocation extras
  ["/Applications/Racket v5.3.4/bin/racket"] + sicp_requires + extras
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

task :build do
 system *sicp_test_invocation
end

namespace :sicp do
  task :shell do
    system *sicp_shell_invocation
  end
end

task :default => :build
