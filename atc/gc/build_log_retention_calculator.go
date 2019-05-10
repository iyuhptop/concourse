package gc

import (
	"github.com/concourse/concourse/atc/db"
)

type BuildLogRetentionCalculator interface {
	BuildLogsToRetain(db.Job) (int, int)
}

type buildLogRetentionCalculator struct {
	defaultBuildLogsToRetain     uint64
	maxBuildLogsToRetain         uint64
	defaultDaysToRetainBuildLogs uint64
	maxDaysToRetainBuildLogs     uint64
}

func NewBuildLogRetentionCalculator(
	defaultBuildLogsToRetain uint64,
	maxBuildLogsToRetain uint64,
	defaultDaysToRetainBuildLogs uint64,
	maxDaysToRetainBuildLogs uint64,
) BuildLogRetentionCalculator {
	return &buildLogRetentionCalculator{
		defaultBuildLogsToRetain:     defaultBuildLogsToRetain,
		maxBuildLogsToRetain:         maxBuildLogsToRetain,
		defaultDaysToRetainBuildLogs: defaultDaysToRetainBuildLogs,
		maxDaysToRetainBuildLogs:     maxDaysToRetainBuildLogs,
	}
}

func (blrc *buildLogRetentionCalculator) BuildLogsToRetain(job db.Job) (int, int) {
	// What does the job want?
	var daysToRetainBuildLogs = 0
	var buildLogsToRetain = 0
	if job.Config().BuildLogRetention != nil {
		daysToRetainBuildLogs = job.Config().BuildLogRetention.Days
		buildLogsToRetain = job.Config().BuildLogRetention.Builds
	} else {
		buildLogsToRetain = job.Config().BuildLogsToRetain
	}

	// If not specified, set to default
	if buildLogsToRetain == 0 {
		buildLogsToRetain = int(blrc.defaultBuildLogsToRetain)
	}
	if daysToRetainBuildLogs == 0 {
		daysToRetainBuildLogs = int(blrc.defaultDaysToRetainBuildLogs)
	}

	// If we don't have a max set, then we're done
	if blrc.maxBuildLogsToRetain == 0 && blrc.maxDaysToRetainBuildLogs == 0 {
		return buildLogsToRetain, daysToRetainBuildLogs
	}

	var buildLogsToRetainReturn int
	var daysToRetainBuildLogsReturn int
	// If we have a value set, and we're less than the max, then return
	if buildLogsToRetain > 0 && buildLogsToRetain < int(blrc.maxBuildLogsToRetain) {
		buildLogsToRetainReturn = buildLogsToRetain
	} else {
		buildLogsToRetainReturn= int(blrc.maxBuildLogsToRetain)
	}

	if daysToRetainBuildLogs > 0 && daysToRetainBuildLogs < int(blrc.maxDaysToRetainBuildLogs) {
		daysToRetainBuildLogsReturn = daysToRetainBuildLogs
	} else {
		daysToRetainBuildLogsReturn = int(blrc.maxDaysToRetainBuildLogs)
	}

	return buildLogsToRetainReturn, daysToRetainBuildLogsReturn

}
