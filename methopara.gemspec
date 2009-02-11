# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{methopara}
  s.version = "0.3.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Koichi Sasada", "Genki Takiuchi"]
  s.date = %q{2009-02-11}
  s.description = %q{Method#parameters for ruby-1.9.1}
  s.email = %q{genki@s21g.com}
  s.extensions = ["ext/extconf.rb"]
  s.extra_rdoc_files = ["README", "ChangeLog"]
  s.files = ["README", "ChangeLog", "Rakefile", "lib/methopara.rb", "ext/methopara.c", "ext/extconf.rb"]
  s.has_rdoc = true
  s.homepage = %q{http://methopara.rubyforge.org}
  s.rdoc_options = ["--title", "methopara documentation", "--charset", "utf-8", "--opname", "index.html", "--line-numbers", "--main", "README", "--inline-source", "--exclude", "^(examples|extras)/"]
  s.require_paths = ["lib"]
  s.required_ruby_version = Gem::Requirement.new("= 1.9.1")
  s.rubyforge_project = %q{methopara}
  s.rubygems_version = %q{1.3.1}
  s.summary = %q{Method#parameters for ruby-1.9.1}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 2

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
    else
    end
  else
  end
end
