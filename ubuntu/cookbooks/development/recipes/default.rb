#
# Cookbook Name:: development
# Recipe:: default
#
# Copyright 2015, south37
#
# All rights reserved - Do Not Redistribute
#

package 'git' do
  action :install
end

package 'haskell-platform' do
  action :install
end

package 'vim' do
  action :install
end

