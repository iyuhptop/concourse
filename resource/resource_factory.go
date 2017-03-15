package resource

import (
	"code.cloudfoundry.org/lager"
	"github.com/concourse/atc"
	"github.com/concourse/atc/dbng"
	"github.com/concourse/atc/worker"
)

//go:generate counterfeiter . ResourceFactoryFactory

type ResourceFactoryFactory interface {
	FactoryFor(workerClient worker.Client) ResourceFactory
}

type resourceFactoryFactory struct{}

func NewResourceFactoryFactory() ResourceFactoryFactory {
	return &resourceFactoryFactory{}
}

func (f *resourceFactoryFactory) FactoryFor(workerClient worker.Client) ResourceFactory {
	return &resourceFactory{
		workerClient: workerClient,
	}
}

//go:generate counterfeiter . ResourceFactory

type ResourceFactory interface {
	NewPutResource(
		logger lager.Logger,
		id worker.Identifier,
		metadata worker.Metadata,
		containerSpec worker.ContainerSpec,
		resourceTypes atc.VersionedResourceTypes,
		imageFetchingDelegate worker.ImageFetchingDelegate,
	) (Resource, error)

	NewCheckResource(
		logger lager.Logger,
		resourceUser dbng.ResourceUser,
		id worker.Identifier,
		metadata worker.Metadata,
		resourceSpec worker.ContainerSpec,
		resourceTypes atc.VersionedResourceTypes,
		imageFetchingDelegate worker.ImageFetchingDelegate,
		resourceConfig atc.ResourceConfig,
	) (Resource, error)
}

type resourceFactory struct {
	workerClient worker.Client
}

func (f *resourceFactory) NewPutResource(
	logger lager.Logger,
	id worker.Identifier,
	metadata worker.Metadata,
	containerSpec worker.ContainerSpec,
	resourceTypes atc.VersionedResourceTypes,
	imageFetchingDelegate worker.ImageFetchingDelegate,
) (Resource, error) {
	container, err := f.workerClient.FindOrCreateBuildContainer(
		logger,
		nil,
		imageFetchingDelegate,
		id,
		metadata,
		containerSpec,
		resourceTypes,
	)
	if err != nil {
		return nil, err
	}

	return NewResourceForContainer(container), nil
}

func (f *resourceFactory) NewCheckResource(
	logger lager.Logger,
	resourceUser dbng.ResourceUser,
	id worker.Identifier,
	metadata worker.Metadata,
	resourceSpec worker.ContainerSpec,
	resourceTypes atc.VersionedResourceTypes,
	imageFetchingDelegate worker.ImageFetchingDelegate,
	resourceConfig atc.ResourceConfig,
) (Resource, error) {
	container, err := f.workerClient.FindOrCreateResourceCheckContainer(
		logger,
		resourceUser,
		nil,
		imageFetchingDelegate,
		id,
		metadata,
		resourceSpec,
		resourceTypes,
		resourceConfig.Type,
		resourceConfig.Source,
	)
	if err != nil {
		return nil, err
	}

	return NewResourceForContainer(container), nil
}
