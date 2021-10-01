#! /usr/bin/env ruby

require "json"
require "open3"
require "rubygems/package"

def pandoc(from_format, to_format, strings)
  cmd = ["pandoc-tar", "--from", from_format, "--to", to_format, "--log", "-"]
  Open3.popen3(*cmd) do |sub_in, sub_out, sub_err, sub_wait|
    Gem::Package::TarWriter.new(sub_in) do |tar|
      strings.each_with_index do |string, i|
        tar.add_file_simple("#{i}", 0, string.length) do |output|
          output.write(string)
        end
      end
    end
    sub_in.close_write
    results = []
    Gem::Package::TarReader.new(StringIO.new(sub_out.read)) do |tar|
      tar.each do |entry|
        results.push(if entry.file? then entry.read else nil end)
      end
    end
    errors = JSON.load(sub_err)
    [results, errors]
  end
end

MARKDOWN = ["# Hello world", "* foo\n* bar"]
results, errors = pandoc("markdown", "html", MARKDOWN)
$stderr.puts(errors.inspect)
results.each do |s| puts s end
