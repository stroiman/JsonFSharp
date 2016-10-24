require 'semver'
v = SemVer.find

task :build do
  sh "xbuild JsonFSharp.sln"
end

task :test => [:build] do
  sh "mono JsonFSharp.Specs/bin/Debug/JsonFSharp.Specs.exe"
end

task :pack => :test do
  sh "nuget pack -Version #{v.format "%M.%m.%p%s%d"} JsonFSharp/JsonFSharp.nuspec"
  sh "git tag #{v.to_s}"
  sh "git push --tags"
  sh "nuget push JsonFSharp.#{v.format "%M.%m.%p%s%d"}.nupkg #{ENV['nuget_apikey']} -Source https://www.nuget.org/api/v2/package"
  v.patch += 1
  v.save
  sh "git add .semver"
  sh "git commit -m \"bump to version #{v.to_s}\""
  sh "git push"
end

task :restore do
  sh "mono paket.bootstrapper.exe"
  sh "mono paket.exe restore"
end

task :ci_build => [:restore, :test]
task :default => :test
