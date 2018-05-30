from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

import os

from pants.backend.jvm.ossrh_publication_metadata import (Developer, License,
                                                          OSSRHPublicationMetadata, Scm)
from pants.backend.jvm.repository import Repository
from pants.build_graph.build_file_aliases import BuildFileAliases


public_repo = Repository(name='public',
                         url='https://oss.sonatype.org/#stagingRepositories',
                         push_db_basedir=os.path.join('build-support', 'ivy', 'pushdb'))

local_repo = Repository(name='local',
                          url='',
                          push_db_basedir=os.path.join('build-support', 'ivy', 'pushdb'))


def org_pantsbuild_publication_metadata(description):
  return OSSRHPublicationMetadata(
    description=description,
    url='https://github.com/ActionIQ-OSS',
    licenses=[
      License(
        name='Apache License, Version 2.0',
        url='http://www.apache.org/licenses/LICENSE-2.0'
      )
    ],
    developers=[
      Developer(
        name='Larry Finn',
        email='larry@actioniq.com',
        url='https://github.com/ActionIQ-OSS'
      )
    ],
  )


def build_file_aliases():
  return BuildFileAliases(
    objects={
      'public': public_repo,  # key 'public' must match name='public' above
      'local': local_repo,
      'pants_library': org_pantsbuild_publication_metadata
    },
)
