package db

import (
	"fmt"
	"time"

	"github.com/concourse/atc"
)

type ContainerIdentifier struct {
	WorkerID int
	// if it's a resource check container
	PipelineID int
	ResourceID int
	// if it's a step container
	BuildID int
	PlanID  atc.PlanID
}

type ContainerMetadata struct {
	BuildID              int
	BuildName            string
	ResourceName         string
	WorkerName           string
	PipelineName         string
	JobName              string
	StepName             string
	Type                 ContainerType
	WorkingDirectory     string
	CheckType            string
	CheckSource          atc.Source
	EnvironmentVariables []string
}

type Container struct {
	ContainerIdentifier
	ContainerMetadata

	ExpiresAt time.Time
	Handle    string
}

type ContainerType string

func (containerType ContainerType) String() string {
	return string(containerType)
}

func ContainerTypeFromString(containerType string) (ContainerType, error) {
	switch containerType {
	case "check":
		return ContainerTypeCheck, nil
	case "get":
		return ContainerTypeGet, nil
	case "put":
		return ContainerTypePut, nil
	case "task":
		return ContainerTypeTask, nil
	default:
		return "", fmt.Errorf("Unrecognized containerType: %s", containerType)
	}
}

const (
	ContainerTypeCheck ContainerType = "check"
	ContainerTypeGet   ContainerType = "get"
	ContainerTypePut   ContainerType = "put"
	ContainerTypeTask  ContainerType = "task"
)
