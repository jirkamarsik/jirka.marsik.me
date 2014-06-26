#!/usr/bin/env bash

select yn in "Yes" "No"; do \
  case $yn in
      Yes ) exit 0;;
      No ) exit 1;;
  esac
done

